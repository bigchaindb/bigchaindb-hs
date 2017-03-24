{-# LANGUAGE OverloadedStrings #-}

module BigchainDB.API where

import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (toStrict)
import Data.Text.Encoding

import BigchainDB.Crypto
import BigchainDB.CryptoConditions
import BigchainDB.Prelude
import qualified BigchainDB.Transaction as TX

import System.IO.Unsafe


type RequestParser r = Object -> Parser (Except String r)
type ApiMethod = BS.ByteString -> IO BS.ByteString


jsonAPI :: (ToJSON result) => RequestParser result -> ApiMethod
jsonAPI parseRequestObject bs = do
  let res = do
               req <- eitherDecodeStrict bs
               act <- parseEither parseRequest req
               runExcept act
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
  , (validateFulfillmentSpec, "validateFulfillmentSpec",
     "Validate fulfillment specification")
  , (parseConditionDSL, "parseConditionDSL",
     "Parse condition DSL into fulfillment spec")
  , (signFulfillmentSpec, "signFulfillmentSpec", "Sign a fulfillment template")
  , (verifyFulfillment, "verifyFulfillment", "Verify a fulfillment payload")
  ]


generateKeyPair :: ApiMethod
generateKeyPair = jsonAPI $ \_ -> return $ do
  let (pk, sk) = unsafePerformIO genKeyPair
  return $ object ["public_key" .= pk , "secret_key" .= sk]


createTx :: ApiMethod
createTx = jsonAPI $ \obj -> do
  TX.mkCreateTx <$> obj .:? "asset" .!= mempty
                <*> obj .:  "creator"
                <*> obj .:  "outputs"
                <*> obj .:? "metadata" .!= mempty


signTx :: ApiMethod
signTx = jsonAPI $ \obj -> do
  anyTx <- obj .: "tx"
  untx <- case anyTx of
               TX.AnyS _ -> fail "Tx is already signed"
               TX.AnyU tx -> pure tx
  TX.signTx <$> obj .: "key" <*> pure untx


validateTx :: ApiMethod
validateTx = jsonAPI $ \obj -> do
  _ <- parseJSON (Object obj) :: Parser TX.AnyTransaction
  return $ return $ object ["result" .= String "ok"]


validateFulfillmentSpec :: ApiMethod
validateFulfillmentSpec = jsonAPI $ \obj -> do
  ffTemplate <- parseJSON (Object obj) :: Parser TX.FulfillmentTemplate
  return $ return $ object ["result" .= toJSON (show ffTemplate)]


parseConditionDSL :: ApiMethod
parseConditionDSL = jsonAPI $ \obj -> do
  expr <- obj .: "expr"
  return $ do
    cond <- parseDSL expr
    return $ object ["result" .= toJSON (TX.FFTemplate cond)]


signFulfillmentSpec :: ApiMethod
signFulfillmentSpec = jsonAPI $ \obj -> do
  (TX.FFTemplate cond) <- obj .: "condition"
  keys <- obj .: "keys" :: Parser [SecretKey]
  msg <- encodeUtf8 <$> obj .: "msg"
  let signed = foldl (TX.signCondition msg) cond keys
      mff = getFulfillmentBase64 signed
      out = maybe (throwE "Could not sign")
                  (\uri -> pure (object ["result" .= uri]))
                  mff
  return out
  

verifyFulfillment :: ApiMethod
verifyFulfillment = jsonAPI $ \obj -> do
  (TX.FFTemplate target) <- obj .: "condition"
  ff <- encodeUtf8 <$> obj .: "fulfillment"
  msg <- encodeUtf8 <$> obj .: "msg"
  return $ do
    ffill <- readStandardFulfillmentBase64 ff
    let valid = validate (getConditionURI target) ffill msg
    pure $ object ["result" .= valid]


readFulfillment :: ApiMethod
readFulfillment = jsonAPI $ \obj -> do
  ff <- encodeUtf8 <$> obj .: "fulfillment"
  --msg <- encodeUtf8 <$> obj .:? "message"
  return $ do
    ffill <- readStandardFulfillmentBase64 ff
    pure $ object ["result" .= TX.FFTemplate ffill]
    --if validate (getConditionURI ffill) ffill msg
    --   then pure $ object ["result" .= TX.FFTemplate ffill]
    --   else throwE "Invalid fulfillment"
             
