
--------------------------------------------------------------------------------
-- Crypto Conditions Standard API
--
-- Targeting spec: draft-thomas-crypto-conditions-02 of December 20, 2016
--
-- The Condition type defined in this module supports the standard
-- condition types, library authors wishing to extend CryptoConditions
-- should copy and paste this file into their own project and define their own
-- Condition type.
--------------------------------------------------------------------------------

module Interledger.CryptoConditions.Standard
  ( module CCI
  , Condition(..)
  , ed25519Condition
  , fulfillEd25519
  , verifyStandard
  , preimageCondition
  ) where

import qualified Crypto.PubKey.Ed25519 as Ed2

import Data.ByteString as BS
import Data.Word
import Data.Text (Text)

import Interledger.CryptoConditions.Impl as CCI


data Condition =
    Threshold Word16 [Condition]
  | Ed25519 Ed2.PublicKey (Maybe Ed2.Signature)
  | Preimage BS.ByteString (Maybe BS.ByteString)
  deriving (Show, Eq)


instance IsCondition Condition where
  getType (Threshold _ _) = thresholdType
  getType (Ed25519 _ _) = ed25519Type
  getType (Preimage _ _) = preimageType
  getCost (Threshold t subs) = thresholdCost t subs
  getCost (Ed25519 _ _) = ed25519Cost
  getCost (Preimage _ (Just pre)) = preimageCost pre
  getCost (Preimage _ _) = error "time to refactor!"
  getFingerprint (Threshold t subs) = thresholdFingerprint t subs
  getFingerprint (Ed25519 pk _) = ed25519Fingerprint pk
  getFingerprint (Preimage hash _) = hash
  getFulfillment (Threshold t subs) = thresholdFulfillment t subs
  getFulfillment (Ed25519 pk msig) = ed25519Fulfillment pk <$> msig
  getFulfillment (Preimage _ mpre) = preimageFulfillment <$> mpre
  getSubtypes (Threshold _ subs) = thresholdSubtypes subs
  getSubtypes _ = mempty
  verifyFf 0 = verifyPreimage
  verifyFf 2 = verifyThreshold
  verifyFf 4 = verifyEd25519


ed25519Condition :: Ed2.PublicKey -> Condition
ed25519Condition pk = Ed25519 pk Nothing

preimageCondition :: BS.ByteString -> Condition
preimageCondition pre = Preimage (sha256 pre) (Just pre)


fulfillEd25519 :: Ed2.PublicKey -> Ed2.Signature
               -> Condition -> Condition
fulfillEd25519 pk sig (Threshold t subs) =
  Threshold t $ fulfillEd25519 pk sig <$> subs
fulfillEd25519 pk sig e@(Ed25519 pk' Nothing) =
  if pk == pk' then Ed25519 pk (Just sig) else e
fulfillEd25519 _ _ c = c


verifyStandard :: BS.ByteString -> BS.ByteString -> Text -> VerifyResult
verifyStandard msg = verifyFulfillment (Verify msg :: Verify Condition)
