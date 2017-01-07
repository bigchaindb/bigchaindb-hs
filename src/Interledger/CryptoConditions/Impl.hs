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


import Control.Monad.Trans.Except

import Crypto.Hash

import Data.ASN1.BinaryEncoding
import Data.ASN1.BinaryEncoding.Raw
import Data.ASN1.BitArray
import Data.ASN1.Encoding
import Data.ASN1.Parse
import Data.ASN1.Types
import qualified Data.Attoparsec.Text as AT
import Data.Bits
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as B64
import Data.List ((\\), sortOn)
import Data.Maybe
import Data.Monoid
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Word

import BigchainDB.Crypto

import Interledger.CryptoConditions.Encoding


--------------------------------------------------------------------------------
-- | Class of things that are conditions
--
class IsCondition c where
  getCost :: c -> Int
  getType :: c -> ConditionType
  getTypeById :: c -> Int -> ConditionType
  getFingerprint :: c -> BS.ByteString
  getFulfillment :: c -> Maybe BS.ByteString
  getSubtypes :: c -> Set.Set ConditionType
  getURI :: c -> T.Text
  getURI = getConditionURI
  verifyFf :: Int -> Verify c -> ParseASN1 BS.ByteString


newtype Verify c = Verify BS.ByteString


data VerifyResult = Passed | Failed String
  deriving (Show, Eq)


encodeConditionASN :: IsCondition c => c -> [ASN1]
encodeConditionASN c =
  let ct = getType c
      fingerprint = getFingerprint c
      costBs = BS.pack $ bytesOfUInt $ fromIntegral $ getCost c
      --mask = typeMask $ getSubtypes c
      --mid = if hasSubtypes ct
      --         then [BitString $ toBitArray mask 0]
      --         else []
   in fiveBellsThingy (typeId ct) [fingerprint, costBs]


encodeCondition :: IsCondition c => c -> BS.ByteString
encodeCondition = encodeASN1' DER . encodeConditionASN


getConditionURI :: IsCondition c => c -> T.Text
getConditionURI c =
  let ct = getType c
      f = decodeUtf8 $ b64EncodeStripped $ getFingerprint c
      cost = T.pack $ show $ getCost c
      subtypes = if hasSubtypes ct
                    then "&subtypes=" <> typeNames (getSubtypes c)
                    else ""
   in "ni:" <> hashFunc ct <> ";" <> f
       <> "?fpt=" <> typeName ct <> "&cost="
       <> cost <> subtypes


getFulfillmentBase64 :: IsCondition c => c -> Maybe T.Text
getFulfillmentBase64 =
  fmap (decodeUtf8 . B64.encode) . getFulfillment


verifyFulfillment :: IsCondition c => Verify c -> BS.ByteString
                  -> T.Text -> VerifyResult
verifyFulfillment v bs = either Failed go . uriGetFingerprint
  where go f1 = case parseASN1 bs (verifyPoly v) of
                     Left err -> Failed err
                     Right f2 ->
                       if f1 == f2
                          then Passed
                          else Failed "fulfillment does not match"


verifyPoly :: IsCondition c => Verify c -> ParseASN1 BS.ByteString
verifyPoly v = do
  asn <- getNext
  case asn of
    (Start c@(Container Context tid)) -> do
      res <- verifyFf tid v
      end <- getNext
      if end /= End c then throwParseError "Failed parsing end"
                      else pure res
    body -> throwParseError ("Failed parsing body: " ++ show body)


--------------------------------------------------------------------------------
-- | Parse a fingerprint from a uri

uriGetFingerprint :: T.Text -> Either String BS.ByteString
uriGetFingerprint t = parse t >>= b64DecodeStripped . encodeUtf8 
  where
    parse = AT.parseOnly $ do
        "ni:" >> AT.skipWhile (/=';') >> ";"
        AT.takeWhile1 (/='?')


--------------------------------------------------------------------------------
-- | Type of a condition
--
data ConditionType = CT
  { typeId :: Int
  , typeName :: T.Text
  , hasSubtypes :: Bool
  , hashFunc :: T.Text
  }
  deriving (Show)


-- Eq and Ord instances consider only the ID
--
instance Eq ConditionType where
  ct == ct' = typeId ct == typeId ct'


instance Ord ConditionType where
  ct <= ct' = typeId ct <= typeId ct'


-- Functions for working with sets of condition types
--
typeNames :: Set.Set ConditionType -> T.Text
typeNames = T.intercalate "," . map typeName . Set.toAscList


typeMask :: Set.Set ConditionType -> BS.ByteString
typeMask cts =
  let bits = Set.map ((2^) . typeId) cts
      mask = foldl (.|.) 0 bits :: Int
   in BS.singleton $ fromIntegral mask


--------------------------------------------------------------------------------
-- | Aggregate condition container.
--

thresholdType :: ConditionType
thresholdType = CT 2 "threshold-sha-256" True "sha-256"


thresholdFulfillment :: IsCondition c => Word16 -> [c] -> Maybe BS.ByteString
thresholdFulfillment t subs =
  let ti = fromIntegral t
      withFf = zip subs (getFulfillment <$> subs)
      byCost = sortOn ffillCost withFf
      ffills = take ti $ catMaybes $ snd <$> byCost
      conds = encodeCondition . fst <$> drop ti byCost
      body = asnSequenceBS [ asnSequenceBS ffills
                           , asnSequenceBS conds
                           ]
      asn = asnChoiceBS (typeId thresholdType) body
      encoded = encodeASN1' DER [asn]
   in if length ffills == ti then Just encoded else Nothing
  where
    -- order by has ffill then cost of ffill
    ffillCost (c, Just _) = (0::Int, getCost c)
    ffillCost _           = (1, 0)


thresholdFingerprint :: IsCondition c => Word16 -> [c] -> BS.ByteString
thresholdFingerprint t subs =
  thresholdFingerprintFromBins t $ encodeCondition <$> subs


thresholdFingerprintFromBins :: Word16 -> [BS.ByteString] -> BS.ByteString
thresholdFingerprintFromBins t subs = 
  let subs' = x690Sort subs
      encoded = asnSequenceBS $
        [ encodeASN1' DER [IntVal $ fromIntegral t]
        , asnSequenceBS subs'
        ]
   in BS.pack $ BA.unpack (hash encoded :: Digest SHA256)


thresholdSubtypes :: IsCondition c => [c] -> Set.Set ConditionType
thresholdSubtypes subs =
  let cts = Set.fromList (getType <$> subs)
      all' = Set.unions (cts:(getSubtypes <$> subs))
   in Set.delete thresholdType all'


thresholdCost :: IsCondition c => Word16 -> [c] -> Int
thresholdCost t subs =
  let largest = take (fromIntegral t) $ sortOn (*(-1)) $ getCost <$> subs
   in sum largest + 1024 * length subs


verifyThreshold :: IsCondition c => Verify c -> ParseASN1 BS.ByteString
verifyThreshold v =
  onNextContainer Sequence $ do
    ffilled <- onNextContainer Sequence $ getMany $ verifyPoly v
    conds <- onNextContainer Sequence $ getMany $
      encodeASN1' DER . (:[]) <$> getNext
    let t = fromIntegral $ length ffilled
    if ffilled \\ conds /= []
       then throwParseError "Threshold does not satisfy"
       else pure $ thresholdFingerprintFromBins t conds


--------------------------------------------------------------------------------
-- | ED25519-SHA256 Condition type
--

ed25519Type :: ConditionType
ed25519Type = CT 4 "ed25519-sha-256" False "sha-256"


ed25519Cost :: Int
ed25519Cost = 131072


ed25519Fingerprint :: PublicKey -> BS.ByteString
ed25519Fingerprint pk = BS.pack $ sha256 body
  where body = encodeASN1' DER asn
        asn = [ Start Sequence
              , Other Context 0 $ toData pk
              ]


ed25519Fulfillment :: PublicKey -> Signature -> BS.ByteString
ed25519Fulfillment pk sig = encodeASN1' DER body
  where body = fiveBellsThingy (typeId ed25519Type) [toData pk, toData sig]


verifyEd25519 :: Verify c -> ParseASN1 BS.ByteString
verifyEd25519 (Verify msg) = do
  elements <- (,) <$> getNext <*> getNext
  (pk, sig) <- case elements of
      (Other Context 0 bspk, Other Context 1 bssig) -> do
        let res = runExcept $ (,) <$> fromData bspk <*> fromData bssig
        either throwParseError return res
      _ -> fail "Ed25519 does not validate"
  if verify pk msg sig
     then pure $ ed25519Fingerprint pk
     else fail "Ed25519 does not validate"


--------------------------------------------------------------------------------
-- Utilities


sha256 :: BA.ByteArrayAccess a => a -> [Word8]
sha256 a = BA.unpack $ (hash a :: Digest SHA256)
