{-# LANGUAGE OverloadedStrings #-}

module BigchainDB.API.Tx where

import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.Map as Map
import Data.Text (pack)
import Data.Text.Encoding

import BigchainDB.API.Http
import BigchainDB.API.Utils
import BigchainDB.Crypto
import BigchainDB.CryptoConditions
import BigchainDB.Prelude
import qualified BigchainDB.Transaction as TX

import Lens.Micro


createTx :: JsonMethod
createTx = pureMethod $ \obj -> do
  act <- TX.mkCreateTx <$> obj .:-? "asset" .!= TX.nullPayload
                       <*> obj .:-  "creator"
                       <*> obj .:-  "outputs"
                       <*> obj .:-? "metadata" .!= TX.nullPayload
  pure $ toJSON <$> act


signTx :: JsonMethod
signTx = pureMethod $ \obj -> do
  tx <- obj .:- "tx"
  keys <- obj .:- "keys" :: Parser [SecretKey]
  check <- obj .:-? "check" .!= True
  pure $ do
    let signed = foldl TX.signTxPartial tx keys
    if check && not (TX.txIsSigned signed)
       then throwE $ errMsg TxSignMissingPrivateKeys "missing private keys"
       else pure $ toJSON signed


validateTx :: JsonMethod
validateTx = pureMethod $ \obj -> do
  txVal <- obj .:- "tx" :: Parser Value
  let res = fromJSON txVal :: Result TX.Transaction
  pure $
    case res of
         Error str -> throwE $ 
             let code = if take 14 str == "expected txid:"
                           then TxWrongId
                           else TxInvalid
             in errMsg code $ pack str
         Success _ -> pure ok


validateCondition :: JsonMethod
validateCondition = pureMethod $ \obj -> do
  _ <- obj .:- "condition" :: Parser TX.Condition
  pure $ pure ok


parseConditionDSL :: JsonMethod
parseConditionDSL = pureMethod $ \obj -> do
  expr <- obj .:- "spec"
  pure $ toJSON . TX.Condition <$> parseDSL expr


signCondition :: JsonMethod
signCondition = pureMethod $ \obj -> do
  (TX.Condition cond) <- obj .:- "condition"
  keys <- obj .:- "keys" :: Parser [SecretKey]
  msg <- encodeUtf8 <$> obj .:- "msg"
  let signed = foldl (TX.signCondition msg) cond keys
      mff = getFulfillmentBase64 signed
      e = errMsg TxSignMissingPrivateKeys "missing private keys"
  pure $ maybe (throwE e) (pure . toJSON) mff


verifyFulfillment :: JsonMethod
verifyFulfillment = pureMethod $ \obj -> do
  (TX.Condition target) <- obj .:- "condition"
  ff <- encodeUtf8 <$> obj .:- "fulfillment"
  msg <- encodeUtf8 <$> obj .:- "msg"
  pure $ do
    ffill <- readStandardFulfillmentBase64 ff
    let valid = validate (getConditionURI target) ffill msg
    pure $ object ["valid" .= valid]


readFulfillment :: JsonMethod
readFulfillment = pureMethod $ \obj -> do
  ff <- encodeUtf8 <$> obj .:- "fulfillment"
  --msg <- encodeUtf8 <$> obj .:-? "message"
  pure $ do
    ffill <- readStandardFulfillmentBase64 ff
    pure $ toJSON $ TX.Condition ffill
    --if validate (getConditionURI ffill) ffill msg
    --   then pure $ object ["result" .= TX.Condition ffill]
    --   else throwE "Invalid fulfillment"


transferTx :: JsonMethod
transferTx = pureMethod $ \obj -> do
  act <- TX.mkTransferTx <$> obj .:-  "spends"
                         <*> obj .:-? "links" .!= mempty
                         <*> obj .:-  "outputs"
                         <*> obj .:-? "metadata" .!= TX.nullPayload
  pure $ toJSON <$> act


ok :: Value
ok = String "ok"
