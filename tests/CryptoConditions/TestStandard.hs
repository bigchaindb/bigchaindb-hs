{-# LANGUAGE OverloadedStrings #-}

module CryptoConditions.TestStandard
  ( standardTests
  ) where

import qualified Crypto.PubKey.Ed25519 as Ed2

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Text as T

import BigchainDB.Crypto
import Interledger.CryptoConditions.Standard


standardTests :: TestTree
standardTests = testGroup "testStandard"
  [ testCase "testFulfill" $ do
      let cond = Threshold 1 [preimageCondition "ah", ed2Alice]
          (Just ffill) = getFulfillment cond
          condUri = getURI <$> readStandardFulfillment "" ffill
       in condUri @?= Right (getURI cond)

  ]



ed2Alice, ed2Bob :: Condition
ed2Alice = ed25519Condition pkAlice
ed2Bob = ed25519Condition pkBob


alice, bob :: T.Text
alice = "7uQSF92GR1ZVmL7wNs3MJcg5Py2sDbpwCBmWNrYVSQs1"
bob = "DCBsceTfmZXL5d9t3enc7VPdYpPETixD12qKXs53oW6Q"


pkAlice, pkBob :: Ed2.PublicKey
pkAlice = let (PK k) = unsafeParseKey alice in k
pkBob = let (PK k) = unsafeParseKey bob in k
