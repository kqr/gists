{-# LANGUAGE OverloadedStrings #-}

{-
 - Desktop skype has ridiculously slow searching, so when I had to use Skype
 - professionally I needed a faster way to search through conversation
 - history. This is not user friendly, but it is fast.
 -}

module Main where

import           Data.Maybe                  (catMaybes)
import           Data.Monoid                 ((<>), mconcat)
import           Data.Text                   (pack, isSuffixOf)
import qualified Data.Text.IO           as T (putStrLn, getLine)
import           Database.SQLite.Simple      (open, query, query_, fromOnly)
import           System.Environment          (getEnv)


{-
 - MISSING
 -----------
 - List possible usernames based on directories?
 - show sent/received markers
 - strip xml?
 - show list in pager?
 - don't limit to OS X
 -----------
 -}


main = do
    db <- open =<< findDb

    partner <- getPartner =<< availablePartners db
    needle <- T.getLine << putStrLn "Search for?..."

    hits <- searchIn db partner needle
    mapM_ T.putStrLn hits


findDb = do
  homeDir <- getEnv "HOME"
  putStrLn "What is your skype username?"
  username <- getLine
  return $ mconcat
      [ homeDir
      , "/Library/Application Support/Skype/"
      , username
      , "/main.db"
      ]



availablePartners db =
    let
        candidates = query_ db $ mconcat
            [ "SELECT DISTINCT dialog_partner "
            , "FROM Messages "
            , "ORDER BY dialog_partner"
            ]
        validUsers = filter (not . isSuffixOf "thread.skype")
    in
        fmap (validUsers . catMaybes . map fromOnly) candidates


getPartner partners = do
    listAlternatives partners
    T.putStrLn "Which conversation?"
    pick <- readLn
    return (partners !! pick)


listAlternatives xs =
    let
        indexed = zip [0..] xs
        formatIxed (i, x) = pack (show i) <> ") " <> x
    in
        mapM (T.putStrLn . formatIxed) indexed


searchIn db partner needle =
    let
        searchQuery = mconcat
            [ "SELECT body_xml "
            , "FROM Messages "
            , "WHERE dialog_partner=? AND body_xml LIKE ? "
            , "ORDER BY timestamp ASC"
            ]
        arguments = (partner, "%" <> needle <> "%")
        results = query db searchQuery arguments
    in
        fmap (map fromOnly) results

