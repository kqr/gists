{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}


{-
 -
 - This is probably a bit useless at the moment because the crawling is
 - untargeted, so it'll generate a bunch of useless links. You probably
 - want to first check whether the link leads to HTML data to get a more
 - sensible set.
 -
 - Oh, and by the way, this is nowhere near finished or polished. It's
 - the result of a couple of hours of hacking and then got abandoned for
 - a more fruitful, targeted (albeit manual) approach.
 -
 -}

import Control.Lens
import Control.Monad
import Data.Text (Text, unpack)
import Data.Text.Lazy.Encoding
import Data.ByteString.Lazy (ByteString)
import Network.Wreq
import Text.Taggy.Lens
import Network.URI
import Data.Maybe
import System.IO
import Data.Either
import Data.Set (Set)
import qualified Data.Set as Set


-- Helper lenses for URI parsing
$(makeLensesFor
    [("uriAuthority", "uriAuthorityL")
    ,("uriScheme", "uriSchemeL")
    ,("uriFragment", "uriFragmentL")
    ] ''URI)



------------------------------------
-- Super special fancy queue object
-- where each element can only exist
-- in it once ever, and it keeps track
-- of which elements has passed
-- through it.

data Uniqueue a =
    Uniqueue
        { visited :: Set a
        , remaining :: Set a
        }
        deriving Show

next :: Ord a => Uniqueue a -> Maybe (a, Uniqueue a)
next uniqueue =
    if Set.null (remaining uniqueue) then
        Nothing
    else
        let
            element = Set.elemAt 0 (remaining uniqueue)
            newRemaining = Set.delete element (remaining uniqueue)
            newVisited = Set.insert element (visited uniqueue)
        in
            Just (element, Uniqueue newVisited newRemaining)

member :: Ord a => a -> Uniqueue a -> Bool
member a uniqueue =
    Set.member a (remaining uniqueue) || Set.member a (visited uniqueue)

insert :: Ord a => a -> Uniqueue a -> Uniqueue a
insert a uniqueue =
    if member a uniqueue then
        uniqueue
    else
        uniqueue { remaining = Set.insert a (remaining uniqueue) }

append :: Ord a => Set a -> Uniqueue a -> Uniqueue a
append aas uniqueue =
    let
        go aas uniqueue =
            case aas of
                [] -> uniqueue
                (a:as) -> go as (insert a uniqueue)
    in
        go (Set.toList aas) uniqueue

-- End of super fancy special queue object
--------------------------------------------




main = do
    let starting = fromJust (parseURI "http://www.sunet.se/")
    crawled <- crawl (Uniqueue Set.empty (Set.singleton starting))
    forM_ (visited crawled) (putStrLn . show)


-- recursively visit all urls in the uniqueue
crawl :: Uniqueue URI -> IO (Uniqueue URI)
crawl links =
    case next links of
        Nothing -> return links
        Just (link, nextUniqueue) -> do
            putStrLn (show link)
            newLinks <- getLinksFor link
            crawl (append newLinks nextUniqueue)


-- get and parse all href attributes for a page, and print errors for the
-- unparsed ones
getLinksFor :: URI -> IO (Set URI)
getLinksFor uri = do
    (broken, working) <- fmap (partitionEithers . extractHrefs) (get (show uri))
    when (not (null broken)) $ do
        hPutStrLn stderr "-----------------"
        hPutStrLn stderr "Unparsed URIs:"
        mapM_ (hPutStrLn stderr) broken
        hPutStrLn stderr "-----------------"
    return (filterInternal (fromJust (uriAuthority uri)) working)


-- add authority to internal links, and throw out links that aren't internal
filterInternal :: URIAuth -> [URI] -> Set URI
filterInternal auth uris =
    let
        internal =
            filter (isInternal auth) uris
        complete =
            internal
                & each . uriAuthorityL .~ Just auth
                & each . uriSchemeL .~ "http:"
                & each . uriFragmentL .~ ""
    in
        Set.fromList complete

-- the path and the query arguments are "the request"
getRequest :: URI -> String
getRequest uri =
    uriPath uri ++ uriQuery uri

-- If the url auth is the same as auth it's an internal link
isInternal :: URIAuth -> URI -> Bool
isInternal auth uri =
    maybe True (== auth) (uriAuthority uri)


-- Extract all the values of all the <a href="____"> attributes
-- on a page and attempt to return them parsed
extractHrefs :: Response ByteString -> [Either String URI]
extractHrefs response =
    let
        document =
            responseBody . to decodeLatin1 . html
        hrefs =
            allNamed (only "a") . attr "href" . _Just
        annotateParseURI str =
            maybe (Left str) Right (parseURIReference str)
    in
        response ^.. document . hrefs . to unpack . to annotateParseURI


