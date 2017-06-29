{-# LANGUAGE OverloadedStrings #-}

module BigchainDB.CryptoConditions
  ( module DSL
  , module BCT
  , getConditionPubkeys
  , getConditionDetails
  , parseConditionDetails
  ) where

import Data.Aeson.Types
import Data.Text as T

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
         , "signature" .= Null
         ]
getConditionDetails (Threshold n subs) = 
  object [ "type" .= String "threshold-sha-256"
         , "threshold" .= n
         , "subfulfillments" .= (getConditionDetails <$> subs)
         ]


parseConditionDetails :: Object -> Parser CryptoCondition
parseConditionDetails obj = do
  condType <- obj .: "type"
  case condType of
       "ed25519-sha-256" -> do
         (PK pk) <- obj .: "public_key"
         pure $ Ed25519 pk Nothing
       "threshold-sha-256" -> do
         subffills <- obj .: "subfulfillments"
         subconds <- mapM parseConditionDetails subffills
         Threshold <$> obj .: "threshold" <*> pure subconds
       _ -> fail ("Unsupported condition type: " ++ condType)
