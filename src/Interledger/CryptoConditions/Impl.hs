{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
-- Crypto Conditions Implementation
--
-- Targeting spec: draft-thomas-crypto-conditions-02 of December 20, 2016
--
-- Design
--
-- This approach to Crypto Conditions in Haskell has the goals:
--
-- * Simple to understand and work with
-- * Easily extensible with custom condition types
--
-- The bottleneck to achieving these goals is polymorphism; Haskell does not
-- support dynamic dispatch like OOP languages, and runtime type casting
-- in Haskell is very unnatural and somewhat unsafe.
--
-- The solution is to decouple the polymorphism from the implementation,
-- such that the core algorithms and serialization can work with instances
-- of an "IsCondition" class, and a polymorphic data type can be implemented
-- separately to support the desired condition types and behaviours.
--
-- The module Interledger.CryptoConditions.Standard supports the standard
-- condition types, library authors wishing to extend CryptoConditions
-- should copy and paste this file into their own project and define their own
-- Condition type.
--------------------------------------------------------------------------------


module Interledger.CryptoConditions.Impl where


import Crypto.Hash

import Data.ASN1.BinaryEncoding
import Data.ASN1.BitArray
import Data.ASN1.Encoding
import Data.ASN1.Types
import Data.Bits
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Base64.URL as B64
import qualified Data.ByteString.Char8 as C8
import Data.List (sortOn)
import Data.Maybe
import Data.Monoid
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Word

import BigchainDB.Crypto


--------------------------------------------------------------------------------
-- | Class of things that are conditions
--
class IsCondition c where
  getCost :: c -> Int
  getType :: c -> ConditionType
  getFingerprint :: c -> BL.ByteString
  getFulfillment :: c -> Maybe ASN1
  getSubtypes :: c -> Set.Set ConditionType
  getURI :: c -> T.Text
  getURI = getConditionURI


getConditionASN :: IsCondition c => c -> ASN1
getConditionASN c =
  let ct = getType c
      fingerprint = BL.toStrict $ getFingerprint c
      mask = typeMask $ getSubtypes c
      mid = if hasSubtypes ct
               then [BitString $ toBitArray mask 0]
               else []
      body = [ Start Sequence
             , OctetString fingerprint
             , IntVal $ fromIntegral $ getCost c
             ] ++ mid ++ [End Sequence]
   in asnChoice (typeID ct) body


getConditionURI :: IsCondition c => c -> T.Text
getConditionURI c =
  let ct = getType c
      f = decodeUtf8 $ b64EncodeStripped $ BL.toStrict $ getFingerprint c
      cost = T.pack $ show $ getCost c
      subtypes = if hasSubtypes ct
                    then "&subtypes=" <> typeNames (getSubtypes c)
                    else ""
   in "ni:///" <> hashFunc ct <> ";" <> f
       <> "?fpt=" <> typeName ct <> "&cost="
       <> cost <> subtypes


getFulfillmentB64 :: IsCondition c => c -> Maybe T.Text
getFulfillmentB64 = fmap enc . getFulfillment
  where enc asn = let bs = BL.toStrict $ encodeASN1 DER [asn]
                   in decodeUtf8 $ B64.encode bs


--------------------------------------------------------------------------------
-- | Type of a condition
--
data ConditionType = CT 
  { typeID :: Int
  , typeName :: T.Text
  , hasSubtypes :: Bool
  , hashFunc :: T.Text
  }
  deriving (Show)


-- Eq and Ord instances consider only the ID
--
instance Eq ConditionType where
  ct == ct' = typeID ct == typeID ct'


instance Ord ConditionType where
  ct <= ct' = typeID ct <= typeID ct'


-- Functions for working with sets of condition types
--
typeNames :: Set.Set ConditionType -> T.Text
typeNames = T.intercalate "," . map typeName . Set.toAscList


typeMask :: Set.Set ConditionType -> BS.ByteString
typeMask cts =
  let bits = Set.map ((2^) . typeID) cts
      mask = foldl (.|.) 0 bits :: Int
   in BS.singleton $ fromIntegral mask


--------------------------------------------------------------------------------
-- | Aggregate condition container.
--
-- Is either a single condition or a threshold condition.
--

thresholdType :: ConditionType
thresholdType = CT 2 "threshold-sha-256" True "sha-256"


thresholdFulfillment :: IsCondition c => Word16 -> [c] -> Maybe ASN1
thresholdFulfillment t subs =
  let ti = fromIntegral t
      withFf = zip subs (getFulfillment <$> subs)
      byCost = sortOn ffillCost withFf
      ffills = take ti $ catMaybes $ snd <$> byCost
      conds = getConditionASN . fst <$> drop ti byCost
      body = [Start Sequence, Start Sequence] ++
             ffills ++
             [End Sequence, Start Sequence] ++
             conds ++
             [End Sequence, End Sequence]
      asn = asnChoice (typeID thresholdType) body
   in if length ffills == ti then Just asn else Nothing
  where
    -- order by has ffill then cost of ffill
    ffillCost (c, Just _) = (0::Int, getCost c)
    ffillCost _           = (1, 0)


thresholdFingerprint :: IsCondition c => Word16 -> [c] -> BL.ByteString
thresholdFingerprint t subs =
  let subs' = x690Sort $ getFingerprint <$> subs
      encoded = encodeASN1 DER
        ([Start Sequence, IntVal (fromIntegral t), Start Sequence] ++
         (OctetString <$> (subs' >>= BL.toChunks)) ++
         [End Sequence, End Sequence])
   in BL.pack $ BA.unpack $ (hashlazy encoded :: Digest SHA256)


thresholdSubtypes :: IsCondition c => [c] -> Set.Set ConditionType
thresholdSubtypes subs =
  let cts = Set.fromList (getType <$> subs)
      all' = Set.unions (cts:(getSubtypes <$> subs))
   in Set.delete thresholdType all'


thresholdCost :: IsCondition c => Word16 -> [c] -> Int
thresholdCost t subs =
  let largest = take (fromIntegral t) $ sortOn (*(-1)) $ getCost <$> subs
   in sum largest + 1024 * length subs


--------------------------------------------------------------------------------
-- | ED25519-SHA256 Condition type
--

ed25519Type :: ConditionType
ed25519Type = CT 4 "ed25519" False "sha-256"


ed25519Cost :: Int
ed25519Cost = 131072


ed25519Fingerprint :: PublicKey -> BL.ByteString
ed25519Fingerprint = BL.pack . sha256


ed25519Fulfillment :: PublicKey -> Signature -> ASN1
ed25519Fulfillment pk sig = asnChoice (typeID ed25519Type)
  [ Start Sequence
  , OctetString $ BS.pack $ BA.unpack pk
  , OctetString $ BS.pack $ BA.unpack sig
  , End Sequence
  ]


--------------------------------------------------------------------------------
-- Utilities

b64EncodeStripped :: BS.ByteString -> BS.ByteString
b64EncodeStripped bs =
  let b64 = B64.encode bs
   in case C8.elemIndex '=' b64 of Just i -> BS.take i b64
                                   Nothing -> b64


sha256 :: BA.ByteArrayAccess a => a -> [Word8]
sha256 a = BA.unpack $ (hash a :: Digest SHA256)


x690Sort :: [BL.ByteString] -> [BL.ByteString]
x690Sort = sortOn (\bs -> (BL.length bs, bs))


asnChoice :: Int -> [ASN1] -> ASN1
asnChoice tid body =
  let encoded = BL.toStrict $ encodeASN1 DER body
   in Other Context tid encoded
