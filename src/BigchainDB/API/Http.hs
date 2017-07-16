{-# LANGUAGE OverloadedStrings #-}

module BigchainDB.API.Http where

import Control.Exception (try)

import Data.Aeson.Types

import Network.HTTP.Simple

import BigchainDB.API.Utils
import BigchainDB.Prelude
import BigchainDB.Transaction.Types



httpMethod :: StrictObject -> String -> String -> (Request -> Request) ->
  Parser (ExceptT Err IO Value)
httpMethod obj path method setup = do
  server <- obj .:- "server"
  pure $ do
    let reqStr = method ++ " " ++ server ++ path
    req <- setup <$> parseRequest reqStr
    response <- wrapHttpErrors $ httpJSONEither req
    case getResponseBody response of
         Left jsonError -> throwE $ errStr HttpJsonError $ show jsonError
         Right r -> pure r


wrapHttpErrors :: Show a => IO (Response a) -> ExceptT Err IO (Response a)
wrapHttpErrors act = do
  eresponse <- lift $ try act
  case eresponse of
       Left e ->
         throwE $ errStr HttpConnectionError $ show $
           (e :: HttpException)
       Right r -> checkResponse r *> pure r


checkResponse :: Show a => Response a -> ExceptT Err IO ()
checkResponse response = do
  let code = getResponseStatusCode response
  when (code == 404) $ do
    throwE $ errStr Http404NotFound $ show response
  when (code - mod code 100 /= 200) $ do
    throwE $ errStr HttpError $ show response


httpGetPath :: JsonMethod
httpGetPath = ioMethod $ \obj -> do
  path <- obj .:- "path"
  httpMethod obj path "GET" id


httpGetTransaction :: JsonMethod
httpGetTransaction = ioMethod $ \obj -> do
  (Txid txid) <- obj .:- "txid"
  let path = "/api/v1/transactions/" ++ unpack txid
  httpMethod obj path "GET" id


httpPostTransaction :: JsonMethod
httpPostTransaction = ioMethod $ \obj -> do
  tx <- obj .:- "tx" :: Parser Transaction
  let setup = setRequestBodyJSON tx
  httpMethod obj "/api/v1/transactions/" "POST" setup
