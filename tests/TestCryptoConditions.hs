{-# LANGUAGE OverloadedStrings #-}

module TestCryptoConditions
  ( cryptoConditionsTests
  ) where


import Control.Monad.Trans.Except

import qualified Data.Set as Set
import Data.Text (Text)

import Test.Tasty
import Test.Tasty.HUnit

import BigchainDB.Crypto
import Interledger.CryptoConditions.Encoding
import Interledger.CryptoConditions.Standard

import CryptoConditions.TestEncoding


cryptoConditionsTests :: TestTree
cryptoConditionsTests = testGroup "CryptoConditions"
  [ uris
  , subTypes
  , testVerify
  , testEncoding
  ]


uris :: TestTree
uris = testGroup "URIs"
  [
    testCase "ed25519-uri" $
      getURI (ed25519Condition pkAlice)
      @?= "ni:///sha-256;vqWqESi-XbzUFLAgaZLV61KAhKbAK94ZX2Is4g1Xs8Q?fpt=ed25519&cost=131072"

  , testCase "threshold-ed25519-uri" $
      let tc = Threshold 2 (ed25519Condition <$> [pkAlice, pkBob])
       in getURI tc 
          @?= "ni:///sha-256;fkq01gavJORqayKOKIAJwkEk31bSzqQ68sQKHHspkmw?fpt=threshold-sha-256&cost=264192&subtypes=ed25519"
  ]


subTypes :: TestTree
subTypes = testGroup "subTypes"
  [
    testCase "threshold" $
      let tc = Threshold 2 (ed25519Condition <$> [pkAlice, pkBob])
       in getSubtypes tc @?= Set.fromList [ed25519Type]

  , testCase "mask" $
       typeMask subtypes @?= "\5" -- 2^0 | 2^2
  
  , testCase "names" $
       typeNames subtypes @?= "a,b"
  ]
  where
    subtypes = Set.fromList [ CT 0 "a" undefined undefined
                            , CT 2 "b" undefined undefined
                            ]


testVerify :: TestTree
testVerify = testGroup "Fulfillment verification"
  [
    testCase "verify ed25519" $
      let sig = sign skBob pkBob "wat"
          cond = fulfillEd25519 pkBob sig $ ed25519Condition pkBob
          Just ffill = getFulfillment cond
          uri = getURI cond
       in Passed @?= verifyStandard "wat" ffill uri
  ]

alice :: Text
alice = "7uQSF92GR1ZVmL7wNs3MJcg5Py2sDbpwCBmWNrYVSQs1"

bob :: Text
bob = "DCBsceTfmZXL5d9t3enc7VPdYpPETixD12qKXs53oW6Q"

pkAlice :: PublicKey
pkAlice = unsafeParseKey alice

pkBob :: PublicKey
pkBob = unsafeParseKey bob

skBob :: SecretKey
skBob = unsafeParseKey "8LDm2sbJNZgQAgDyE8GEEY7eYLsXk4oe1sBS4NPLfTL"
