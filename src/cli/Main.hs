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
import qualified BigchainDB.FFI as API
import BigchainDB.Prelude

import System.Exit
import System.IO


parseCmd :: Parser (IO C8.ByteString)
parseCmd = subparser $
  foldl1 (<>) $ (\(c,(m,h)) -> apiMethod m c h) <$> methods
  where
    methods = Map.toList API.methods
    apiMethod m c h = command c $ info (parseMethod m) (progDesc h)
    parseMethod m = m . C8.pack <$> argument str (metavar "JSON")


parseOpts :: ParserInfo (Bool, IO C8.ByteString)
parseOpts = info (parser <**> helper) desc
  where
    parser = (,) <$> pretty <*> parseCmd
    pretty = switch (long "pretty" <> help "Pretty print output")
    desc = fullDesc <> progDesc "BigchainDB Clientside API"


main :: IO ()
main = do
  (pretty, act) <- execParser parseOpts
  rjson <- C8L.fromStrict <$> act
  let (Just val) = decode rjson
      out = if pretty then encodePretty' pconf val else rjson
      err = parseMaybe (\o -> o .: "error") val :: Maybe String
  when (isJust err) $ C8L.hPutStrLn stderr out >> exitFailure
  C8L.putStrLn out
  where
    pconf = defConfig { confCompare=compare, confIndent=Spaces 2 }
