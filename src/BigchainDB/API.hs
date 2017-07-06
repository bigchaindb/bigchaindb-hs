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

type JsonMethod = Value -> ExceptT BDBError IO Value


methods :: Map.Map String (JsonMethod, String)
methods = Map.fromList
  [ ("generateKeyPair", (generateKeyPair, "Generate ed25519 key pair"))
  , ("createTx", (createTx, "Generate a CREATE transaction"))
  , ("signTx", (signTx, "Sign a transaction"))
  , ("validateTx", (validateTx, "Validate a transaction (not against DB)"))
  , ("validateCondition", (validateCondition, "Validate condition"))
  , ("parseConditionDSL", (parseConditionDSL, "Parse condition DSL into Condition"))
  , ("signCondition", (signCondition, "Sign a condition"))
  , ("readFulfillment", (readFulfillment, "Get condition from fulfillment"))
  , ("verifyFulfillment", (verifyFulfillment, "Verify a fulfillment payload"))
  ]


runJsonRpc :: Value -> ExceptT BDBError IO Value
runJsonRpc val = do
  let res = parseEither parseRequest val
  (name, params) <- ExceptT $ pure $ over _Left invalidRequest res
  runMethod name params
  where
    invalidRequest str = BDBError (-32600) Null str
    parseRequest = withObject "request" $ \obj ->
      (,) <$> obj .: "method" <*> obj .: "params"


runMethod :: String -> Value -> ExceptT BDBError IO Value
runMethod name params = do
  (method,_) <- maybe (throwE $ methodNotFound name) pure $
                       Map.lookup name methods
  method params
  where
    methodNotFound name = BDBError (-32601) (toJSON name) "Method not found"


wrapJson :: Either BDBError Value -> Value
wrapJson = either wrapJsonError wrapSuccess
  where
    wrapSuccess val = object ["result" .= val]


wrapJsonError :: BDBError -> Value
wrapJsonError (BDBError code val msg) =
   let err = object ["code" .= code, "message" .= msg, "data" .= val]
    in object ["error" .= err]


ok :: Value
ok = String "ok"


params :: (Object -> Parser (Except BDBError Value)) -> Value -> ExceptT BDBError IO Value
params parse val = do
  let res = parseEither (withObject "object" parse) val
  act <- either (throwE . invalidParams) pure res
  either throwE pure $ runExcept act
  where
    invalidParams str = BDBError (-32602) Null str


generateKeyPair :: JsonMethod
generateKeyPair _ = do
  (pk, sk) <- lift genKeyPair
  return $ object ["public_key" .= pk, "secret_key" .= sk]


createTx :: JsonMethod
createTx = params $ \obj -> do
  act <- TX.mkCreateTx <$> obj .:? "asset" .!= TX.emptyObject
                       <*> obj .:  "creator"
                       <*> obj .:  "outputs"
                       <*> obj .:? "metadata" .!= TX.emptyObject
  pure $ toJSON <$> act


signTx :: JsonMethod
signTx = params $ \obj -> do
  anyTx <- obj .: "tx"
  key <- obj .: "key"
  untx <- case anyTx of
               TX.AnyS _ -> fail "Tx is already signed"
               TX.AnyU tx -> pure tx
  pure $ toJSON <$> TX.signTx key untx


validateTx :: JsonMethod
validateTx = params $ \obj -> do
  txVal <- obj .: "tx" :: Parser Value
  let res = fromJSON txVal :: Result TX.AnyTransaction
  pure $
    case res of
         Error str -> throwE $ badTx str
         Success _ -> pure ok


validateCondition :: JsonMethod
validateCondition = params $ \obj -> do
  _ <- parseJSON (Object obj) :: Parser TX.Condition
  pure $ pure ok


parseConditionDSL :: JsonMethod
parseConditionDSL = params $ \obj -> do
  expr <- obj .: "expr"
  pure $ toJSON . TX.Condition <$> parseDSL expr


signCondition :: JsonMethod
signCondition = params $ \obj -> do
  (TX.Condition cond) <- obj .: "condition"
  keys <- obj .: "keys" :: Parser [SecretKey]
  msg <- encodeUtf8 <$> obj .: "msg"
  let signed = foldl (TX.signCondition msg) cond keys
      mff = getFulfillmentBase64 signed
  pure $ maybe (throwE missingPrivateKeys) (pure . toJSON) mff


verifyFulfillment :: JsonMethod
verifyFulfillment = params $ \obj -> do
  (TX.Condition target) <- obj .: "condition"
  ff <- encodeUtf8 <$> obj .: "fulfillment"
  msg <- encodeUtf8 <$> obj .: "msg"
  pure $ do
    ffill <- readStandardFulfillmentBase64 ff
    let valid = validate (getConditionURI target) ffill msg
    pure $ object ["valid" .= valid]


readFulfillment :: JsonMethod
readFulfillment = params $ \obj -> do
  ff <- encodeUtf8 <$> obj .: "fulfillment"
  --msg <- encodeUtf8 <$> obj .:? "message"
  pure $ do
    ffill <- readStandardFulfillmentBase64 ff
    pure $ toJSON $ TX.Condition ffill
    --if validate (getConditionURI ffill) ffill msg
    --   then pure $ object ["result" .= TX.Condition ffill]
    --   else throwE "Invalid fulfillment"
