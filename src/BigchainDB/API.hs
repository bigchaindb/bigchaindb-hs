{-# LANGUAGE OverloadedStrings #-}

module BigchainDB.API
  ( module API
  , methods
  , runJsonRpc
  , runMethod
  , wrapJson
  ) where

import Data.Aeson.Types
import qualified Data.Map as Map

import Lens.Micro

import BigchainDB.API.Http as API
import BigchainDB.API.Tx as API
import BigchainDB.API.Utils as API
import BigchainDB.Crypto
import BigchainDB.Prelude


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
  , ("httpGetPath", (httpGetPath, "GET /some/path from a server"))
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


generateKeyPair :: JsonMethod
generateKeyPair _ = do
  (pk, sk) <- lift genKeyPair
  return $ object ["public_key" .= pk, "secret_key" .= sk]


showErrorClasses :: JsonMethod
showErrorClasses _ = return $ object ["errors" .= allErrorClasses]
