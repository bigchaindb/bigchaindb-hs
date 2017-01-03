{-# LANGUAGE OverloadedStrings #-}

module BigchainDB.API where

import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (toStrict)

import BigchainDB.Crypto
import BigchainDB.Prelude
import qualified BigchainDB.Transaction as TX


type RequestParser r = Object -> Parser (ExceptT String IO r)
type ApiMethod = BS.ByteString -> IO BS.ByteString


jsonAPI :: (ToJSON result) => RequestParser result -> ApiMethod
jsonAPI parseRequestObject bs = do
  res <- runExceptT $ do
    req <- ExceptT $ return $ eitherDecodeStrict bs
    act <- ExceptT $ return $ parseEither parseRequest req
    act
  return $ toStrict $ encode $
    case res of
         Left err -> object ["error" .= toJSON err]
         Right val -> toJSON val
  where
    parseRequest = withObject "object" parseRequestObject


methods :: [(ApiMethod, String, String)]
methods =
  [ (generateKeyPair, "generateKeyPair", "Generate a ed25519 keys")
  , (createTx, "createTx", "Generate a CREATE transaction")
  , (signTx, "signTx", "Sign a transaction")
  , (validateTx, "validateTx", "Validate a transaction (not against DB)")
  ]


generateKeyPair :: ApiMethod
generateKeyPair = jsonAPI $ \_ -> return $ do
  (pk, sk) <- lift genKeyPair
  return $ object ["public_key" .= pk , "secret_key" .= sk]


createTx :: ApiMethod
createTx = jsonAPI $ \obj -> do
  act <- TX.mkCreateTx <$> obj .:? "asset" .!= mempty
                       <*> obj .:  "creator"
                       <*> obj .:  "outputs"
                       <*> obj .:? "metadata" .!= mempty
  return $ ExceptT $ return $ runExcept act


signTx :: ApiMethod
signTx = jsonAPI $ \obj -> do
  anyTx <- obj .: "tx"
  untx <- case anyTx of
               TX.AnyS _ -> fail "Tx is already signed"
               TX.AnyU tx -> pure tx
  act <- TX.signTx <$> obj .: "key" <*> pure untx
  return $ ExceptT $ return $ runExcept act


validateTx :: ApiMethod
validateTx = jsonAPI $ \obj -> do
  _ <- parseJSON (Object obj) :: Parser TX.AnyTransaction
  return $ return $ object ["result" .= String "ok"]
