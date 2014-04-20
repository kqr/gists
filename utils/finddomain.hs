
-- Massively parallel command-line util to check for available domains.
-- Do not use unless you want to get blacklisted from whois servers
-- very quickly.

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import Options.Applicative hiding ((&))

import Control.Monad (replicateM, when, unless, forM_)

import Data.List (intercalate)
import Data.List.Split (splitOn)
import Text.Regex.TDFA ((=~))

a & f = f a

type RequestChannel = Chan (Either String String)
type ResultChannel = Chan (Either String (String, Bool))



data Configuration = Configuration
    { namelength :: Int
    , tlds :: String
    , fromstdin :: Bool
    , letters :: String
    , workercount :: Int
    , delay :: Int
    , verbose :: Bool
    }


defaultConfig = Configuration
    { namelength = 3
    , tlds = "com,net,org,biz,pro,tel,cat,int"
    , fromstdin = False
    , letters = "abcdefghijklmnopqrstuvwxyz1234567890"
    , workercount = 4
    , delay = 1
    , verbose = False
    }


optparser :: ParserInfo Configuration
optparser = info (helper <*> configparser) (fullDesc <> header "finddomain – Find out which domains are available")
  where configparser = pure Configuration
          <*> option    (long "length"  <> value (defaultConfig & namelength)  <> short 's' <> metavar "N"                 <> help "Sets the desired length for domain names")
          <*> strOption (long "tlds"    <> value (defaultConfig & tlds)        <> short 't' <> metavar "tld1,tld2,..."     <> help "The TLDs being searched through")
          <*> switch    (long "input"   <> value (defaultConfig & fromstdin)   <> short 'i'                                <> help "Turns on checking of domain names from stdin")
          <*> strOption (long "letters" <> value (defaultConfig & letters)                  <> metavar "LETTERS"           <> help "The allowed letters in the domain name")
          <*> option    (long "workers" <> value (defaultConfig & workercount) <> short 'W' <> metavar "N"                 <> help "The more workers, the faster it gets done")
          <*> option    (long "delay"   <> value (defaultConfig & delay)       <> short 'd' <> metavar "N"                 <> help "Make the program pause for N seconds between lookups")
          <*> switch    (long "verbose" <> value (defaultConfig & verbose)                                                 <> help "Set verbose output")



main :: IO ()
main = do
  -- set configuration from command-line options
  config <- execParser optparser

  requests <- newChan
  results <- newChan

  -- 1. spawn off the work producer
  forkIO $ requestMaker config requests

  -- 2. spawn off all the workers
  forM_ [1.. config & workercount] . const . forkIO $
      checkDomain config requests results

  -- 3. and collect the results!
  collect config results
                                                                      

requestMaker :: Configuration -> RequestChannel -> IO ()
requestMaker config requests = do
  -- the generator for random domain names
  let subdomainGen = return $ replicateM (config & namelength) (config & letters)

  -- if the user wants to input domain names themselves, let them
  subdomains <- if config & fromstdin then lines <$> getContents else subdomainGen
                            
  -- create domains from combining domain names with TLDs                                          
  let domains = map (intercalate ".") $ sequence [subdomains, splitOn "," $ config & tlds]

  -- for each domain, request a whois on it and sleep a little
  forM_ domains $ \domain -> do
    writeChan requests (Right domain)
    threadDelay . (*1000000) $ config & delay

  -- then tell all the workers they are done
  forM_ [1.. config & workercount] $ \_ ->
    writeChan requests (Left "done")


collect :: Configuration -> ResultChannel -> IO ()
collect config results = do
  result <- readChan results

  case result of
      -- we got a result to report!
      Right (domain, unused) -> do
          let available domain = putStrLn $ domain ++ " is available!"
              -- only tell about unavailable domains in verbose mode
              unavailable domain = when (config & verbose) $ putStrLn (domain ++ " is NOT available!")
              action = if unused then available else unavailable
          -- report about the domain
          action domain
          -- and collect the rest of the results
          collect config results

      -- if we're done, get outta here
      Left "done" -> return ()
      -- if a worker went wrong and verbose mode is on, crash and burn
      Left s -> when (config & verbose) (fail s)


checkDomain :: Configuration -> RequestChannel -> ResultChannel -> IO ()
checkDomain config requests results = do
  request <- readChan requests

  case request of
    -- if whois on a domain is requested
    Right domain -> do
      -- check the availability through whois
      result <- domainAvailable domain

      case result of
        -- if we got a reply, forward it to the collector and continue checking for requests
        Right unused -> writeChan results (Right (domain, unused)) >> recurse
        -- if domainAvailable broke and we're verbose, crash, otherwise continue
        Left s -> writeChan results (Left s) >> unless (config & verbose) recurse

    -- if we're done, forward to collector and be done with it
    Left "done" ->
      writeChan results (Left "done")

  where
    recurse = checkDomain config requests results


domainAvailable :: String -> IO (Either String Bool)
domainAvailable domain = do
  -- run whois for the domain and save the result
  (exitcode, response, _) <- readProcessWithExitCode "whois" [domain] ""
  let result = response =~ "[Nn]o [Mm]atch|NO MATCH|[Nn]ot [Ff]ound|NOT FOUND|[Nn]ot [Aa]vailable|NOT AVAILABLE"

  return $ case exitcode of
    ExitSuccess   -> Right result
    ExitFailure 1 -> Right result

    ExitFailure 127 ->
        Left $ "Util 'whois' could not be run!"
    ExitFailure 2 ->
        Left $ "whois limit exceeded for " ++ domain ++ "."
    ExitFailure i ->
        Left $ "'whois " ++ domain ++ "' terminated unexpectedly with error code " ++ show i ++ "."

 
