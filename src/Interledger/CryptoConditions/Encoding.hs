
module Interledger.CryptoConditions.Encoding where


import Data.ASN1.BinaryEncoding
import Data.ASN1.BinaryEncoding.Raw
import Data.ASN1.Encoding
import Data.ASN1.Parse
import Data.ASN1.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Base64.URL as B64
import qualified Data.ByteString.Char8 as C8
import Data.List (sortOn)


import BigchainDB.Crypto


b64EncodeStripped :: BS.ByteString -> BS.ByteString
b64EncodeStripped bs =
  let b64 = B64.encode bs
   in case C8.elemIndex '=' b64 of Just i -> BS.take i b64
                                   Nothing -> b64


x690Sort :: [BL.ByteString] -> [BL.ByteString]
x690Sort = sortOn (\bs -> (BL.length bs, bs))


asnChoice :: Int -> [ASN1Event] -> BL.ByteString
asnChoice tid body =
  let header = ASN1Header Context tid True $ LenShort 1
   in toLazyByteString $ Header header : body


asnSequence :: [ASN1Event] -> BS.ByteString
asnSequence items = toByteString $ Header header : items
  where
    header = ASN1Header Universal 0x10 True len
    len = mkSmallestLength 7 -- $ length items


asnPrim :: ASN1 -> [ASN1Event]
asnPrim asn = encodeToRaw [asn]


bsPrim :: BS.ByteString -> [ASN1Event]
bsPrim bs = [Primitive bs]


bslPrim :: BL.ByteString -> ASN1Event
bslPrim = Primitive . BL.toStrict


keyPrim :: B58ED2Key k => k -> [ASN1Event]
keyPrim = asnPrim . OctetString . toData


mkSmallestLength :: Int -> ASN1Length
mkSmallestLength i
  | i < 0x80  = LenShort i
  | otherwise = LenLong (nbBytes i) i
  where nbBytes nb = if nb > 255 then 1 + nbBytes (nb `div` 256) else 1


parseASN1 :: BS.ByteString -> ParseASN1 a -> Either String a
parseASN1 bs act = showErr decoded >>= runParseASN1 act
  where decoded = decodeASN1 DER $ BL.fromStrict bs
        showErr = either (Left . show) Right
