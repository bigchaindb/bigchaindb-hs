{-# LANGUAGE OverloadedStrings #-}

module BigchainDB.API.Http where

import Data.Aeson.Types

import Network.HTTP.Simple

import BigchainDB.API.Utils
import BigchainDB.Prelude
import BigchainDB.Transaction.Types



httpMethod :: Object -> String -> String -> (Request -> Request) ->
  Parser (ExceptT Err IO Value)
httpMethod obj path method setup = do
  server <- obj .: "server"
  pure $ do
    let wat = method ++ " " ++ server ++ path
    req <- setup <$> parseRequest wat
    lift $ getResponseBody <$> httpJSON req


httpGetPath :: JsonMethod
httpGetPath = ioMethod $ \obj -> do
  path <- obj .: "path"
  httpMethod obj path "GET" id


httpGetTransaction :: JsonMethod
httpGetTransaction = ioMethod $ \obj -> do
  (Txid txid) <- obj .: "txid"
  let path = "api/v1/transactions/" ++ unpack txid
  httpMethod obj path "GET" id


httpPostTransaction :: JsonMethod
httpPostTransaction = ioMethod $ \obj -> do
  tx <- obj .: "tx" :: Parser Transaction
  let setup = setRequestBodyJSON tx
  httpMethod obj "api/v1/transactions/" "POST" setup
