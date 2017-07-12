{-# LANGUAGE OverloadedStrings #-}

module BigchainDB.API.Http where

import Data.Aeson.Types

import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Simple

import BigchainDB.API.Utils
import BigchainDB.Prelude



httpGetPath :: JsonMethod
httpGetPath = ioMethod $ \obj -> do
  path <- obj .: "path"
  serverUrl <- obj .: "server_url"
  pure $ do
    req <- parseRequest $ serverUrl ++ path
    lift $ getResponseBody <$> httpJSON req

