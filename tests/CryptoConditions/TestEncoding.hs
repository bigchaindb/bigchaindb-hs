{-# LANGUAGE OverloadedStrings #-}

module CryptoConditions.TestEncoding where


import Data.ASN1.BinaryEncoding
import Data.ASN1.BinaryEncoding.Raw
import Data.ASN1.BitArray
import Data.ASN1.Encoding
import Data.ASN1.Parse
import Data.ASN1.Types

import qualified Data.Set as Set
import Data.Text (Text)

import Test.Tasty
import Test.Tasty.HUnit

import BigchainDB.Crypto
import Interledger.CryptoConditions.Encoding


testEncoding :: TestTree
testEncoding = testGroup "testEncoding"
  [
    testCase "compareAsnSequence" $ do
      let asn = OctetString "hello"
          res = encodeASN1' DER [Start Sequence, asn, End Sequence]
       in asnSequence (asnPrim asn) @?= res

  ,
    testCase "decodeAsnSequence" $
      let asn = OctetString "hello"
          bs = asnSequence (asnPrim asn)
          parsed = parseASN1 bs $ onNextContainer Sequence getNext 
       in parsed @?= Right asn
  ]
 
