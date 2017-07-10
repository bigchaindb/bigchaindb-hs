{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module BigchainDB.CryptoConditions.Types (
    module IE
  , CryptoCondition(..)
  , ed25519Condition
  , fulfillEd25519
  , readStandardFulfillmentBase64
  , validate
  ) where

import qualified Crypto.PubKey.Ed25519 as Ed2

import qualified Data.Set as Set
import Data.Word

import Network.CryptoConditions.Impl
import qualified Network.CryptoConditions.Impl as IE
  (getConditionURI, getFulfillmentBase64, readFulfillmentBase64)

import BigchainDB.Prelude

data CryptoCondition =
    Threshold Word16 [CryptoCondition]
  | Ed25519 Ed2.PublicKey (Maybe Ed2.Signature)
  | Anon Int Fingerprint Int (Set.Set ConditionType)
  deriving (Show, Eq)


instance IsCondition CryptoCondition where
  getType (Anon 2 _ _ _) = thresholdType
  getType (Anon 4 _ _ _) = ed25519Type
  getType (Threshold _ _) = thresholdType
  getType (Ed25519 _ _) = ed25519Type

  getCost (Threshold t subs) = thresholdCost t subs
  getCost (Ed25519 _ _) = ed25519Cost
  getCost (Anon _ _ c _) = c

  getFingerprint (Threshold t subs) = thresholdFingerprint t subs
  getFingerprint (Ed25519 pk _) = ed25519Fingerprint pk
  getFingerprint (Anon _ fp _ _) = fp

  getFulfillment (Threshold t subs) = thresholdFulfillment t subs
  getFulfillment (Ed25519 pk msig) = ed25519Fulfillment pk <$> msig
  getFulfillment (Anon _ _ _ _) = Nothing

  getSubtypes (Threshold _ sts) = thresholdSubtypes sts
  getSubtypes (Anon _ _ _ sts) = sts
  getSubtypes _                = mempty

  parseFulfillment 2 = parseThreshold Threshold
  parseFulfillment 4 = parseEd25519 (\a b -> Ed25519 a (Just b))

  verifyMessage (Threshold m subs) = verifyThreshold m subs      
  verifyMessage (Ed25519 pk (Just sig)) = verifyEd25519 pk sig   
  verifyMessage _ = const False                                  

  anon t f c = Anon t f c . toConditionTypes


toConditionTypes :: Set.Set Int -> Set.Set ConditionType
toConditionTypes = Set.map $
  let u = undefined in (\tid -> getType $ Anon tid u u u)


ed25519Condition :: Ed2.PublicKey -> CryptoCondition
ed25519Condition pk = Ed25519 pk Nothing


fulfillEd25519 :: Ed2.PublicKey -> Ed2.Signature
               -> CryptoCondition -> CryptoCondition
fulfillEd25519 pk sig (Threshold t subs) =
  Threshold t $ fulfillEd25519 pk sig <$> subs
fulfillEd25519 pk sig e@(Ed25519 pk' Nothing) =
  if pk == pk' then Ed25519 pk (Just sig) else e
fulfillEd25519 _ _ c = c


readStandardFulfillmentBase64 :: Fulfillment -> Except Err CryptoCondition
readStandardFulfillmentBase64 = either throw pure . readFulfillmentBase64
  where throw = throwE . errStr TxInvalidFulfillment
