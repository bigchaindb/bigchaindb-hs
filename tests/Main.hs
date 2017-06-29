{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit

import qualified Crypto.PubKey.Ed25519 as Ed2

import Data.Aeson.Quick
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Monoid
import Data.Text (Text)

import Lens.Micro
import Lens.Micro.Aeson

import BigchainDB.API
import BigchainDB.CryptoConditions
import BigchainDB.Crypto
import BigchainDB.Prelude
import BigchainDB.Transaction


main :: IO ()
main = defaultMain $ testGroup "Tests" [ txTests
                                       , dslParserTests
                                       , dslSerializerTests
                                       ]


txTests :: TestTree
txTests = testGroup "Test Transaction ID validation"
  [
     testCase "tx-right-id-succeeds" $ do
         let res = runExcept $ do
               tx <- createTx createSpec
               validateTx $ "{tx}" .% tx
         res @?= Right (String "ok")
  ,
     testCase "tx-wrong-id-fails" $ do
         let res = runExcept $ do
               tx <- createTx createSpec
               validateTx $ "{tx}" .% (build "{id}" tx badId)
         res @?= Left (BDBError 100 Null "Txid mismatch")
  ]
  where
    badId = "FFFd1a44abcf0a18b7aec2d406c11ed0cb0bd371847145be7822c76077ca5514" :: Text
    createSpec = object ["creator" .= alice,
                         "outputs" .= toJSON [["1","(1 of " <> alice <> ")"]]]



dslParserTests :: TestTree
dslParserTests = testGroup "CryptoConditions DSL Parser"
  [
    testCase "ed25519-simple" $
      runExcept (parseDSL alice) @?= Right ed2Alice

  , testCase "ed25519-fail-b58" $
      runExcept (parseDSL "0uQSF92GR1ZVmL7wNs3MJcg5Py2sDbpwCBmWNrYVSQs1")
      @?= Left (conditionDslParseError "Failed reading: Expected \"({num} of ...\" or ed25519 key at char 0")

  , testCase "threshold-simple" $
      runExcept (parseDSL ("(2 of " <> alice <> ", " <> bob <> " * 2)"))
      @?= Right (Threshold 2 [ed2Alice, ed2Bob, ed2Bob])

  , testCase "threshold-empty" $
      runExcept (parseDSL "(1 of )") @?=
        Left (conditionDslParseError "Failed reading: Expected \"({num} of ...\" or ed25519 key at char 6")

  , testCase "threshold low" $
      runExcept (parseDSL $ "(0 of " <> alice <> ")")
      @?= Left (conditionDslParseError "Failed reading: Illegal threshold: 0 at char 6")
  , testCase "threshold high" $
      runExcept (parseDSL $ "(3 of " <> alice <> "," <> bob <> ")")
      @?= Left (conditionDslParseError "Failed reading: Impossible threshold at char 96")
      
  ]


dslSerializerTests :: TestTree
dslSerializerTests = testGroup "CryptoConditions DSL Serializer"
  [ 
    testCase "simple" $
      serializeDSL (Threshold 2 [ed2Alice, ed2Bob, ed2Bob])
      @?= ("(2 of %0, %1, %1)", [PK pkAlice, PK pkBob])
  ]


alice, bob :: Text
alice = "3fyz4CveiiUvQKpjwFS3ghEBXNVkKrFbaEfzHRXKP4MH"
bob = "7q5ci6QWS7RfxdAtsAnvxc5ySiZCzepxKzfaoJwzqc6q"


ed2Alice, ed2Bob :: CryptoCondition
ed2Alice = ed25519Condition pkAlice
ed2Bob = ed25519Condition pkBob


pkAlice, pkBob :: Ed2.PublicKey
[PK pkAlice, PK pkBob] = unsafeParseKey <$> [alice, bob]
