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
import qualified Data.ByteString.Lazy as BL
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
  getFingerprint :: c -> BL.ByteString
  getFulfillment :: c -> Maybe BL.ByteString
  getSubtypes :: c -> Set.Set ConditionType
  getURI :: c -> T.Text
  getURI = getConditionURI
  verifyFf :: Int -> Verify c -> ParseASN1 BL.ByteString


newtype Verify c = Verify BS.ByteString 


data VerifyResult = Passed | Failed String
  deriving (Show, Eq)


encodeCondition :: IsCondition c => c -> BL.ByteString
encodeCondition c =
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
   in asnChoice (typeID ct) $ bsPrim $ asnSequence $ concat $ asnPrim <$> body


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


getFulfillmentBase64 :: IsCondition c => c -> Maybe T.Text
getFulfillmentBase64 =
  fmap (decodeUtf8 . B64.encode . BL.toStrict) . getFulfillment


verifyFulfillment :: IsCondition c => Verify c -> BL.ByteString
                  -> T.Text -> VerifyResult
verifyFulfillment v bsl = either Failed go . uriGetFingerprint
  where bs = BL.toStrict bsl
        go f1 = case parseASN1 bs (verifyPoly v) of
                     Left err -> Failed err
                     Right f2 ->
                       if BL.fromStrict f1 == f2
                          then Passed
                          else Failed "fulfillment does not match"


verifyPoly :: IsCondition c => Verify c -> ParseASN1 BL.ByteString
verifyPoly v = do
  asn <- getNext
  case asn of
    (Other Context typeId bs') ->
        let parse = verifyFf typeId v
            unEither = either throwParseError pure
         in unEither $ parseASN1 bs' parse
    _ -> throwParseError "Failed parsing body"


--------------------------------------------------------------------------------
-- | Parse a fingerprint from a uri

uriGetFingerprint :: T.Text -> Either String BS.ByteString
uriGetFingerprint t = (encodeUtf8 . pad) <$> parse t >>= B64.decode
  where
    parse = AT.parseOnly $ do
        "ni:///" >> AT.skipWhile (/=';') >> ";"
        AT.takeWhile1 (/='?')
    pad fi = let r = 4 - mod (T.length fi) 4
                 n = if r == 4 then 0 else r
              in fi <> T.replicate n "="


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

thresholdType :: ConditionType
thresholdType = CT 2 "threshold-sha-256" True "sha-256"


thresholdFulfillment :: IsCondition c => Word16 -> [c] -> Maybe BL.ByteString
thresholdFulfillment t subs =
  let ti = fromIntegral t
      withFf = zip subs (getFulfillment <$> subs)
      byCost = sortOn ffillCost withFf
      ffills = take ti $ catMaybes $ snd <$> byCost
      conds = encodeCondition . fst <$> drop ti byCost
      body = asnSequence ((bsPrim $ asnSequence $ bslPrim <$> ffills)
                          ++ (bsPrim $ asnSequence $ bslPrim <$> conds))
      asn = asnChoice (typeID thresholdType) $ bsPrim body
   in if length ffills == ti then Just asn else Nothing
  where
    -- order by has ffill then cost of ffill
    ffillCost (c, Just _) = (0::Int, getCost c)
    ffillCost _           = (1, 0)


thresholdFingerprint :: IsCondition c => Word16 -> [c] -> BL.ByteString
thresholdFingerprint t subs =
  thresholdFingerprintFromBins t $ encodeCondition <$> subs


thresholdFingerprintFromBins :: Word16 -> [BL.ByteString] -> BL.ByteString
thresholdFingerprintFromBins t subs = 
  let subs' = x690Sort subs
      encoded = asnSequence $ concat
        [ asnPrim $ IntVal $ fromIntegral t
        , bsPrim $ asnSequence $ bslPrim <$> subs'
        ]
   in BL.pack $ BA.unpack (hash encoded :: Digest SHA256)


thresholdSubtypes :: IsCondition c => [c] -> Set.Set ConditionType
thresholdSubtypes subs =
  let cts = Set.fromList (getType <$> subs)
      all' = Set.unions (cts:(getSubtypes <$> subs))
   in Set.delete thresholdType all'


thresholdCost :: IsCondition c => Word16 -> [c] -> Int
thresholdCost t subs =
  let largest = take (fromIntegral t) $ sortOn (*(-1)) $ getCost <$> subs
   in sum largest + 1024 * length subs


verifyThreshold :: IsCondition c => Verify c -> ParseASN1 BL.ByteString
verifyThreshold v =
  onNextContainer Sequence $ do
    ffilled <- onNextContainer Sequence $ getMany $ verifyPoly v
    conds <- onNextContainer Sequence $ getMany $
      encodeASN1 DER . (:[]) <$> getNext
    let t = fromIntegral $ length ffilled
    if ffilled \\ conds /= []
       then throwParseError "Threshold does not satisfy"
       else pure $ thresholdFingerprintFromBins t conds


--------------------------------------------------------------------------------
-- | ED25519-SHA256 Condition type
--

ed25519Type :: ConditionType
ed25519Type = CT 4 "ed25519" False "sha-256"


ed25519Cost :: Int
ed25519Cost = 131072


ed25519Fingerprint :: PublicKey -> BL.ByteString
ed25519Fingerprint = BL.pack . sha256


ed25519Fulfillment :: PublicKey -> Signature -> BL.ByteString
ed25519Fulfillment pk sig = asnChoice (typeID ed25519Type) body
  where body = bsPrim $ asnSequence $ concat [keyPrim pk, keyPrim sig]


verifyEd25519 :: (PublicKey -> BL.ByteString) -> Verify c
              -> ParseASN1 BL.ByteString
verifyEd25519 encode (Verify msg) = do
  elements <- getNextContainer Sequence
  (pk, sig) <- case elements of
      [OctetString bspk, OctetString bssig] -> do
        let res = runExcept $ (,) <$> fromData bspk <*> fromData bssig
        either throwParseError return res
      _ -> fail "Ed25519 does not validate"
  if verify pk msg sig
     then pure $ encode pk
     else fail "Ed25519 does not validate"


--------------------------------------------------------------------------------
-- Utilities


sha256 :: BA.ByteArrayAccess a => a -> [Word8]
sha256 a = BA.unpack $ (hash a :: Digest SHA256)
