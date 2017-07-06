{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as C8L

import Data.Aeson
import Data.Aeson.Types hiding (Parser)
import Data.Aeson.Encode.Pretty
import qualified Data.Map as Map
import Data.Maybe

import Options.Applicative

import qualified BigchainDB.API as API
import BigchainDB.Prelude

import System.Exit
import System.IO


type Method = ExceptT BDBError IO Value

parseCmd :: Parser Method
parseCmd = subparser $
  foldl1 (<>) $ (\(c,(_,h)) -> apiMethod c h) <$> methods
  where
    methods = Map.toList API.methods
    apiMethod c h = command c $ info (parseMethod c) (progDesc h)
    parseMethod c = API.runMethod c <$> argument jsonArg (metavar "JSON")


jsonArg :: ReadM Value
jsonArg = eitherReader $ eitherDecode . C8L.pack



parseOpts :: ParserInfo (Bool, Method)
parseOpts = info (parser <**> helper) desc
  where
    parser = (,) <$> pretty <*> parseCmd
    pretty = switch (long "pretty" <> help "Pretty print output")
    desc = fullDesc <> progDesc "BigchainDB Clientside API"


main :: IO ()
main = do
  (pretty, act) <- execParser parseOpts
  res <- runExceptT act
  case res of
       Left err -> print err >> exitFailure
       Right val -> do
          let enc = if pretty then encodePretty' pconf else encode
          C8L.putStrLn $ enc val
  where
    pconf = defConfig { confCompare=compare, confIndent=Spaces 2 }
