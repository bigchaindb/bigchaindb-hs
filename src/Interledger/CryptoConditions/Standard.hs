
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
  , preimageCondition
  , fulfillEd25519
  , readStandardFulfillment
  ) where

import qualified Crypto.PubKey.Ed25519 as Ed2

import Data.ByteString as BS
import Data.Word

import Interledger.CryptoConditions.Impl as CCI


data Condition =
    Threshold Word16 [Condition]
  | Ed25519 Ed2.PublicKey (Maybe Ed2.Signature)
  | Preimage BS.ByteString
  | Anon Int BS.ByteString Int
  deriving (Show, Eq)


instance IsCondition Condition where
  getType (Anon 0 _ _) = preimageType
  getType (Anon 2 _ _) = thresholdType
  getType (Anon 4 _ _) = ed25519Type
  getType (Threshold _ _) = thresholdType
  getType (Ed25519 _ _) = ed25519Type
  getType (Preimage _) = preimageType
  getCost (Threshold t subs) = thresholdCost t subs
  getCost (Ed25519 _ _) = ed25519Cost
  getCost (Preimage pre) = preimageCost pre
  getCost (Anon _ _ c) = c
  getFingerprint (Threshold t subs) = thresholdFingerprint t subs
  getFingerprint (Ed25519 pk _) = ed25519Fingerprint pk
  getFingerprint (Preimage pre) = sha256 pre
  getFingerprint (Anon _ fp _) = fp
  getFulfillment (Threshold t subs) = thresholdFulfillment t subs
  getFulfillment (Ed25519 pk msig) = ed25519Fulfillment pk <$> msig
  getFulfillment (Preimage pre) = Just $ preimageFulfillment pre
  getFulfillment (Anon _ _ _) = Nothing
  getSubtypes (Threshold _ subs) = thresholdSubtypes subs
  getSubtypes _ = mempty
  parseFulfillment 0 = verifyPreimage
  parseFulfillment 2 = verifyThreshold Threshold
  parseFulfillment 4 = verifyEd25519
  anon = Anon


preimageCondition :: BS.ByteString -> Condition
preimageCondition = Preimage


ed25519Condition :: Ed2.PublicKey -> Condition
ed25519Condition pk = Ed25519 pk Nothing


fulfillEd25519 :: Ed2.PublicKey -> Ed2.Signature
               -> Condition -> Condition
fulfillEd25519 pk sig (Threshold t subs) =
  Threshold t $ fulfillEd25519 pk sig <$> subs
fulfillEd25519 pk sig e@(Ed25519 pk' Nothing) =
  if pk == pk' then Ed25519 pk (Just sig) else e
fulfillEd25519 _ _ c = c


readStandardFulfillment :: Message -> Fulfillment -> Either String Condition
readStandardFulfillment = readFulfillment
