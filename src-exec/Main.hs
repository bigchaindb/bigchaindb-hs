{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as C8L

import Data.Aeson
import Data.Aeson.Types hiding (Parser(..))
import Data.Aeson.Encode.Pretty

import Data.Maybe

import Options.Applicative

import qualified BigchainDB.API as API
import BigchainDB.Prelude

import System.Exit
import System.IO


parseCmd :: Parser (IO C8.ByteString)
parseCmd = subparser $
  foldl1 (<>) $ (\(m,c,h) -> apiMethod m c h) <$> API.methods
  where
    apiMethod m c h = command c $ info (parseMethod m) (progDesc h)
    parseMethod m = m . C8.pack <$> argument str (metavar "JSON")


parseOpts :: ParserInfo (Bool, IO C8.ByteString)
parseOpts = info ((,) <$> pretty <*> parseCmd) fullDesc
  where
    pretty = switch (long "pretty" <> help "Pretty print output")


main :: IO ()
main = do
  (pretty, act) <- execParser parseOpts
  rjson <- C8L.fromStrict <$> act
  let (Just value) = decode rjson
      out = if pretty then encodePretty' pconf value else rjson
      err = parseMaybe (\o -> o .: "error") value :: Maybe String
  when (isJust err) $ C8L.hPutStrLn stderr out >> exitFailure
  C8L.putStrLn out
  where
    pconf = defConfig { confCompare=compare, confIndent=Spaces 2 }
