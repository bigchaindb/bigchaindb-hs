{-# LANGUAGE OverloadedStrings #-}

module BigchainDB.API where

import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.Map as Map
import Data.Text (pack)
import Data.Text.Encoding

import BigchainDB.Crypto
import BigchainDB.CryptoConditions
import BigchainDB.Prelude
import qualified BigchainDB.Transaction as TX

import Lens.Micro

type JsonMethod = Value -> ExceptT Err IO Value


methods :: Map.Map String (JsonMethod, String)
methods = Map.fromList
  [ ("generateKeyPair", (generateKeyPair, "Generate ed25519 key pair"))
  , ("createTx", (createTx, "Generate a CREATE transaction"))
  , ("transferTx", (transferTx, "Generate a TRANSFER transaction"))
  , ("signTx", (signTx, "Sign a transaction"))
  , ("validateTx", (validateTx, "Validate a transaction (not against DB)"))
  , ("validateCondition", (validateCondition, "Validate condition"))
  , ("parseConditionDSL", (parseConditionDSL, "Parse condition DSL into Condition"))
  , ("signCondition", (signCondition, "Sign a condition"))
  , ("readFulfillment", (readFulfillment, "Get condition from fulfillment"))
  , ("verifyFulfillment", (verifyFulfillment, "Verify a fulfillment payload"))
  , ("showErrorClasses", (showErrorClasses, "Show all error classes"))
  ]


runJsonRpc :: Value -> ExceptT Err IO Value
runJsonRpc val = do
  let res = parseEither parseRequest val
  (name, params) <- ExceptT $ pure $ over _Left invalidRequest res
  runMethod name params
  where
    invalidRequest = errStr InvalidProtocol
    parseRequest = withObject "request" $ \obj ->
      (,) <$> obj .: "method" <*> obj .: "params"


runMethod :: String -> Value -> ExceptT Err IO Value
runMethod name params = do
  let throw = throwE $ errStr InvalidMethod name
  (method,_) <- maybe throw pure $ Map.lookup name methods
  method params


wrapJson :: Either Err Value -> Value
wrapJson = either wrapJsonError wrapSuccess
  where
    wrapSuccess val = object ["result" .= val]
    wrapJsonError val = object ["error" .= val]


ok :: Value
ok = String "ok"


getParams :: (Object -> Parser (Except Err Value)) -> Value -> ExceptT Err IO Value
getParams parse val = do
  let res = parseEither (withObject "object" parse) val
  act <- either (throwE . errStr InvalidParams) pure res
  either throwE pure $ runExcept act


generateKeyPair :: JsonMethod
generateKeyPair _ = do
  (pk, sk) <- lift genKeyPair
  return $ object ["public_key" .= pk, "secret_key" .= sk]


createTx :: JsonMethod
createTx = getParams $ \obj -> do
  act <- TX.mkCreateTx <$> obj .:? "asset" .!= TX.nullPayload
                       <*> obj .:  "creator"
                       <*> obj .:  "outputs"
                       <*> obj .:? "metadata" .!= TX.nullPayload
  pure $ toJSON <$> act


signTx :: JsonMethod
signTx = getParams $ \obj -> do
  tx <- obj .: "tx"
  key <- obj .: "key"
  pure $ toJSON <$> TX.signTx key tx


validateTx :: JsonMethod
validateTx = getParams $ \obj -> do
  txVal <- obj .: "tx" :: Parser Value
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
validateCondition = getParams $ \obj -> do
  _ <- parseJSON (Object obj) :: Parser TX.Condition
  pure $ pure ok


parseConditionDSL :: JsonMethod
parseConditionDSL = getParams $ \obj -> do
  expr <- obj .: "expr"
  pure $ toJSON . TX.Condition <$> parseDSL expr


signCondition :: JsonMethod
signCondition = getParams $ \obj -> do
  (TX.Condition cond) <- obj .: "condition"
  keys <- obj .: "keys" :: Parser [SecretKey]
  msg <- encodeUtf8 <$> obj .: "msg"
  let signed = foldl (TX.signCondition msg) cond keys
      mff = getFulfillmentBase64 signed
      e = errMsg TxSignMissingPrivateKeys "missing private keys"
  pure $ maybe (throwE e) (pure . toJSON) mff


verifyFulfillment :: JsonMethod
verifyFulfillment = getParams $ \obj -> do
  (TX.Condition target) <- obj .: "condition"
  ff <- encodeUtf8 <$> obj .: "fulfillment"
  msg <- encodeUtf8 <$> obj .: "msg"
  pure $ do
    ffill <- readStandardFulfillmentBase64 ff
    let valid = validate (getConditionURI target) ffill msg
    pure $ object ["valid" .= valid]


readFulfillment :: JsonMethod
readFulfillment = getParams $ \obj -> do
  ff <- encodeUtf8 <$> obj .: "fulfillment"
  --msg <- encodeUtf8 <$> obj .:? "message"
  pure $ do
    ffill <- readStandardFulfillmentBase64 ff
    pure $ toJSON $ TX.Condition ffill
    --if validate (getConditionURI ffill) ffill msg
    --   then pure $ object ["result" .= TX.Condition ffill]
    --   else throwE "Invalid fulfillment"


transferTx :: JsonMethod
transferTx = getParams $ \obj -> do
  act <- TX.mkTransferTx <$> obj .:  "spends"
                         <*> obj .:? "links" .!= mempty
                         <*> obj .:  "outputs"
                         <*> obj .:? "metadata" .!= TX.nullPayload
  pure $ toJSON <$> act


showErrorClasses :: JsonMethod
showErrorClasses _ = return $ object ["errors" .= allErrorClasses]
