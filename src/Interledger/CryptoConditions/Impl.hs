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
import qualified Crypto.PubKey.Ed25519 as Ed2

import Data.ASN1.BinaryEncoding
import Data.ASN1.BinaryEncoding.Raw
import Data.ASN1.Encoding
import Data.ASN1.Parse
import Data.ASN1.Types
import qualified Data.Attoparsec.Text as AT
import Data.Bits
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as B64
import Data.List (sortOn)
import Data.Maybe
import Data.Monoid
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Word

import Interledger.CryptoConditions.Encoding

import Debug.Trace

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
      subtypes = "\a" <> (typeMask $ getSubtypes c)
      body = [fingerprint, costBs] ++
             if hasSubtypes ct then [subtypes] else []
   in fiveBellsThingy (typeId ct) body


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
  let op i = shiftL 1 (7 - mod i 8)
      bits = Set.map (op . typeId) cts
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
      conds = encodeConditionASN . fst <$> drop ti byCost
      ffills' = either (error . show) id . decodeASN1' DER <$> ffills
      asn = asnSeq (Container Context 2) $
              asnSeq (Container Context 0) (concat ffills') ++
              asnSeq (Container Context 1) (concat conds)
      encoded = encodeASN1' DER asn
   in if length ffills == ti then Just encoded else Nothing
  where
    -- order by has ffill then cost of ffill
    ffillCost (c, Just _) = (0::Int, getCost c)
    ffillCost _           = (1, 0)


thresholdFingerprint :: IsCondition c => Word16 -> [c] -> BS.ByteString
thresholdFingerprint t subs =
  let asns = encodeConditionASN <$> subs
   in thresholdFingerprintFromAsns t asns


thresholdFingerprintFromAsns :: Word16 -> [[ASN1]] -> BS.ByteString
thresholdFingerprintFromAsns t asns = 
  let subs' = x690SortAsn asns
      c = Container Context 1
      asn = asnSequence $
        [ Other Context 0 (BS.pack $ bytesOfUInt $ fromIntegral t)
        , Start c
        ] ++ concat subs' ++ [End c]
   in sha256 $ encodeASN1' DER asn


thresholdSubtypes :: IsCondition c => [c] -> Set.Set ConditionType
thresholdSubtypes subs =
  let cts = Set.fromList (getType <$> subs)
      all' = Set.unions (cts : (getSubtypes <$> subs))
   in Set.delete thresholdType all'


thresholdCost :: IsCondition c => Word16 -> [c] -> Int
thresholdCost t subs =
  let largest = take (fromIntegral t) $ sortOn (*(-1)) $ getCost <$> subs
   in sum largest + 1024 * length subs


verifyThreshold :: IsCondition c => Verify c -> ParseASN1 BS.ByteString
verifyThreshold v = do
  ffills' <- onNextContainer (Container Context 0) $ getMany $ verifyPoly v
  conds <- onNextContainer (Container Context 1) $ getMany getNext
  let t = fromIntegral $ length ffills'
      err = either (error . show) pure
  decoded <- mapM (err . decodeASN1' DER) ffills'
  pure $ thresholdFingerprintFromAsns t (decoded ++ ((:[]) <$> conds))


--------------------------------------------------------------------------------
-- | ED25519-SHA256 Condition type
--

ed25519Type :: ConditionType
ed25519Type = CT 4 "ed25519-sha-256" False "sha-256"


ed25519Cost :: Int
ed25519Cost = 131072


ed25519Fingerprint :: Ed2.PublicKey -> BS.ByteString
ed25519Fingerprint pk = sha256 body
  where body = encodeASN1' DER asn
        asn = [ Start Sequence
              , Other Context 0 $ toData pk
              ]


ed25519Fulfillment :: Ed2.PublicKey -> Ed2.Signature -> BS.ByteString
ed25519Fulfillment pk sig = encodeASN1' DER body
  where body = fiveBellsThingy (typeId ed25519Type) [toData pk, toData sig]


verifyEd25519 :: Verify c -> ParseASN1 BS.ByteString
verifyEd25519 (Verify msg) = do
  elements <- (,) <$> getNext <*> getNext
  (pk, sig) <- case elements of
      (Other Context 0 bspk, Other Context 1 bssig) -> do
        let res = (,) <$> toKey (Ed2.publicKey bspk)
                      <*> toKey (Ed2.signature bssig)
        either throwParseError return res
      _ -> fail "Ed25519 does not parse"
  if Ed2.verify pk msg sig
     then pure $ ed25519Fingerprint pk
     else fail "Ed25519 does not validate"


--------------------------------------------------------------------------------
-- | Preimage Condition type
--

preimageType :: ConditionType
preimageType = CT 0 "preimage-sha-256" False "sha-256"


preimageFulfillment :: BS.ByteString -> BS.ByteString
preimageFulfillment pre = encodeASN1' DER body
  where body = fiveBellsThingy (typeId preimageType) [pre]


preimageCost :: BS.ByteString -> Int
preimageCost = BS.length 


verifyPreimage :: Verify c -> ParseASN1 BS.ByteString
verifyPreimage _ = do
  element <- getNext
  case element of 
    (Other Context 0 preimage) -> pure $ sha256 preimage
    _                          -> throwParseError "Preimage does not parse"

--------------------------------------------------------------------------------
-- Utilities


sha256 :: BA.ByteArrayAccess a => a -> BS.ByteString
sha256 a = BS.pack $ BA.unpack $ (hash a :: Digest SHA256)
