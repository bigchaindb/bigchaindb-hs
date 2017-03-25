{-# LANGUAGE OverloadedStrings #-}

module BigchainDB.API where

import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Text.Encoding

import BigchainDB.Crypto
import BigchainDB.CryptoConditions
import BigchainDB.Prelude
import BigchainDB.Exceptions
import qualified BigchainDB.Transaction as TX

import Lens.Micro

import System.IO.Unsafe


type JsonMethod = Object -> Parser (Except BDBError Value)


methods :: Map.Map Text (JsonMethod, Text)
methods = Map.fromList
  [ ("generateKeyPair", (generateKeyPair, "Generate ed25519 key pair"))
  , ("createTx", (createTx, "Generate a CREATE transaction"))
  , ("signTx", (signTx, "Sign a transaction"))
  , ("validateTx", (validateTx, "Validate a transaction (not against DB)"))
  , ("validateCondition", (validateCondition, "Validate condition"))
  , ("parseConditionDSL", (parseConditionDSL, "Parse condition DSL into Condition"))
  , ("signFulfillmentSpec", (signFulfillmentSpec, "Sign a fulfillment template"))
  , ("verifyFulfillment", (verifyFulfillment, "Verify a fulfillment payload"))
  ]


ok :: Value
ok = String "ok"


generateKeyPair :: JsonMethod
generateKeyPair _ = do
  let (pk, sk) = unsafePerformIO genKeyPair
  return $ return $ object ["public_key" .= pk , "secret_key" .= sk]


createTx :: JsonMethod
createTx obj = do
  act <- TX.mkCreateTx <$> obj .:? "asset" .!= mempty
                       <*> obj .:  "creator"
                       <*> obj .:  "outputs"
                       <*> obj .:? "metadata" .!= mempty
  pure $ toJSON <$> act


signTx :: JsonMethod
signTx obj = do
  anyTx <- obj .: "tx"
  key <- obj .: "key"
  untx <- case anyTx of
               TX.AnyS _ -> fail "Tx is already signed"
               TX.AnyU tx -> pure tx
  pure $ toJSON <$> TX.signTx key untx


validateTx :: JsonMethod
validateTx obj = do
  _ <- parseJSON (Object obj) :: Parser TX.AnyTransaction
  pure $ pure ok


validateCondition :: JsonMethod
validateCondition obj = do
  _ <- parseJSON (Object obj) :: Parser TX.Condition
  pure $ pure ok


parseConditionDSL :: JsonMethod
parseConditionDSL obj = do
  expr <- obj .: "expr"
  pure $ toJSON . TX.Condition <$> parseDSL expr


signFulfillmentSpec :: JsonMethod
signFulfillmentSpec obj = do
  (TX.Condition cond) <- obj .: "condition"
  keys <- obj .: "keys" :: Parser [SecretKey]
  msg <- encodeUtf8 <$> obj .: "msg"
  let signed = foldl (TX.signCondition msg) cond keys
      mff = getFulfillmentBase64 signed
      out = maybe (throwE missingPrivateKeys)
                  (\uri -> pure (toJSON $ uri))
                  mff
  return out


verifyFulfillment :: JsonMethod
verifyFulfillment obj = do
  (TX.Condition target) <- obj .: "condition"
  ff <- encodeUtf8 <$> obj .: "fulfillment"
  msg <- encodeUtf8 <$> obj .: "msg"
  return $ do
    ffill <- readStandardFulfillmentBase64 ff
    let valid = validate (getConditionURI target) ffill msg
    pure $ object ["result" .= valid]


readFulfillment :: JsonMethod
readFulfillment obj = do
  ff <- encodeUtf8 <$> obj .: "fulfillment"
  --msg <- encodeUtf8 <$> obj .:? "message"
  return $ do
    ffill <- readStandardFulfillmentBase64 ff
    pure $ toJSON $ TX.Condition ffill
    --if validate (getConditionURI ffill) ffill msg
    --   then pure $ object ["result" .= TX.Condition ffill]
    --   else throwE "Invalid fulfillment"
