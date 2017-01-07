{-# LANGUAGE OverloadedStrings #-}

module CryptoConditions.TestFiveBells
  ( fiveBellsSuite
  ) where


import Control.Monad.Trans.Except

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.ByteArray as BA
import Data.ASN1.Encoding 
import Data.ASN1.BinaryEncoding
import Data.ASN1.Types
import Data.Aeson
import Data.Quickson
import Data.Monoid
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

import Lens.Micro
import Lens.Micro.Aeson

import System.IO.Unsafe

import BigchainDB.Crypto
import Interledger.CryptoConditions.Standard
import Interledger.CryptoConditions.Encoding


fiveBellsSuite :: TestTree
fiveBellsSuite = testGroup "fiveBellsSuites"
  [ testMinimalEd25519
  , testBasicThreshold
  --, testHashing
  ]


suiteJson :: FromJSON a => FilePath -> a
suiteJson file = unsafePerformIO $ do
  let path = "ext/five-bells-condition/testsuite/valid/" <> file
  (Just val) <- decodeStrict <$> BS.readFile path
  pure val


fromB16 :: T.Text -> BS.ByteString    
fromB16 t = let (r,"") = B16.decode $ encodeUtf8 t
             in r

fromB64 :: T.Text -> BS.ByteString    
fromB64 = either error id . b64DecodeStripped . encodeUtf8


fromB64Key :: B58ED2Key k => T.Text -> k
fromB64Key k = either error id $ (runExcept . fromData $ fromB64 k)


qu :: FromJSON a => Value -> BS.ByteString -> a
qu val q = either error id $ quicksonParse q >>= flip quicksonExecute val


testMinimalEd25519 :: TestTree
testMinimalEd25519 = testGroup f
  [ testCase "binary condition" $ encodeCondition cond @?= condBin
  , testCase "uri" $ getURI cond @?= condUri
  , testCase "fulfillment" $ getFulfillment cond @?= Just ffillment
  , testCase "verify" $
      verifyStandard (encodeUtf8 msg) ffillment condUri @?= Passed
  ]
  where
    f = "0004_test-minimal-ed25519.json"
    val = suiteJson f
    pub = fromB64Key $ qu val "{json:{publicKey}}"
    sig = fromB64Key $ qu val "{json:{signature}}"
    condBin = fromB16 $ qu val "{conditionBinary}"
    ffillment = fromB16 $ qu val "{fulfillment}"
    (msg,condUri) = qu val "{message,conditionUri}"
    cond = fulfillEd25519 pub sig $ ed25519Condition pub


testBasicThreshold :: TestTree
testBasicThreshold = testCase f $ do
  let (Just cost) = val ^? key "cost" . _Integral
      ffillment = fromB16 $ val ^. key "fulfillment" . _String
  print ffillment 
  where
    f = "0008_test-basic-threshold.json"
    val = suiteJson f :: Value


testHashing = testCase "testHashing" $ do
  let pk = fromB16 "ec172b93ad5e563bf4932c70e1245034c35467ef2efd4d64ebf819683467e2bf"
  print ""
  print pk
  let (Right asn) = B64.decode "MCKAIOwXK5OtXlY79JMscOEkUDTDVGfvLv1NZOv4GWg0Z+K/"
  print $ decodeASN1 DER $ BL.fromStrict asn
  let fing = fromB64 "U1YhFdW0lOI-SVF3PbDP4t_lVefj_-tB5P11yvfBaoE"
  print fing
  print $ BS.pack $ sha256 asn

  
