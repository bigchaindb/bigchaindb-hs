{-# LANGUAGE OverloadedStrings #-}

module BigchainDB.CryptoConditions
  ( module DSL
  , module BCT
  , conditionIsSigned
  , getConditionPubkeys
  , getConditionDetails
  , parseConditionDetails
  , parsePolyFulfillment
  ) where

import Data.Aeson.Types
import Data.Text.Encoding (encodeUtf8)

import BigchainDB.Data.Aeson
import BigchainDB.Crypto
import BigchainDB.CryptoConditions.DSL as DSL
import BigchainDB.CryptoConditions.Types as BCT



getConditionPubkeys :: CryptoCondition -> [PublicKey]
getConditionPubkeys (Ed25519 pk _) = [PK pk]
getConditionPubkeys (Threshold _ cs) = cs >>= getConditionPubkeys
getConditionPubkeys _ = []


getConditionDetails :: CryptoCondition -> Value
getConditionDetails (Ed25519 pk _) =
  object [ "type" .= ("ed25519-sha-256" :: String)
         , "public_key" .= PK pk
         ]
getConditionDetails (Threshold n subs) = 
  object [ "type" .= String "threshold-sha-256"
         , "threshold" .= n
         , "subconditions" .= (getConditionDetails <$> subs)
         ]
getConditionDetails _ = undefined


parseConditionDetails :: Value -> Parser CryptoCondition
parseConditionDetails = withStrictObject "a" $ \obj -> do
  condType <- obj .:- "type"
  case condType of
       "ed25519-sha-256" -> do
         (PK pk) <- obj .:- "public_key"
         pure $ Ed25519 pk Nothing
       "threshold-sha-256" -> do
         subconds <- obj .:- "subconditions" >>= mapM parseConditionDetails
         Threshold <$> obj .:- "threshold" <*> pure subconds
       _ -> fail ("Unsupported condition type: " ++ condType)


parsePolyFulfillment :: Value -> Parser CryptoCondition
parsePolyFulfillment val =
  case val of (Object _) -> parseConditionDetails val
              (String t) -> let econd = readFulfillmentBase64 (encodeUtf8 t)
                            in either fail pure econd
              _          -> typeMismatch "object or string" val


conditionIsSigned :: CryptoCondition -> Bool
conditionIsSigned (Threshold _ cs) = all conditionIsSigned cs
conditionIsSigned (Ed25519 _ (Just _)) = True
conditionIsSigned _ = False

