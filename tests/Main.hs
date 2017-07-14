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

import BigchainDB.API as API
import BigchainDB.CryptoConditions
import BigchainDB.Crypto
import BigchainDB.Prelude
import BigchainDB.Transaction

import System.Random

import Debug.Trace
import Control.Concurrent

main :: IO ()
main = defaultMain $ testGroup "Tests" [ txTests
                                       , dslParserTests
                                       , dslSerializerTests
                                       , httpClientTest
                                       ]


txTests :: TestTree
txTests = testGroup "Test Transaction ID validation"
  [
     testCase "tx-right-id-succeeds" $ do
       res <- runExceptT $ do 
         tx <- createTx createSpec
         validateTx $ "{tx}" .% tx
       res @?= Right (String "ok")


  ,  testCase "tx-no-id-fails" $ do
       res <- runExceptT $ do
         tx <- createTx createSpec
         let txNoId = tx & key "id" .~ Null
         validateTx $ "{tx}" .% txNoId
       res @?= Left (errMsg TxInvalid "failed to parse field id: expected Text, encountered Null")

  ,
     testCase "tx-wrong-id-fails" $ do
       res <- runExceptT $ do
         tx <- createTx createSpec
         validateTx $ "{tx}" .% (build "{id}" tx badId)
       res @?= Left (errMsg TxWrongId "expected txid: 63cfbf6decfdfe1155cfc670f94e3cb889a509001551428dc5cb909b1a78b5e0")
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
      @?= Left (errMsg TxConditionParseError "Failed reading: Expected \"({num} of ...\" or ed25519 key at char 0")

  , testCase "threshold-simple" $
      runExcept (parseDSL ("(2 of " <> alice <> ", " <> bob <> " * 2)"))
      @?= Right (Threshold 2 [ed2Alice, ed2Bob, ed2Bob])

  , testCase "threshold-empty" $
      runExcept (parseDSL "(1 of )") @?=
        Left (errMsg TxConditionParseError "Failed reading: Expected \"({num} of ...\" or ed25519 key at char 6")

  , testCase "threshold low" $
      runExcept (parseDSL $ "(0 of " <> alice <> ")")
      @?= Left (errMsg TxConditionParseError "Failed reading: Illegal threshold: 0 at char 6")
  , testCase "threshold high" $
      runExcept (parseDSL $ "(3 of " <> alice <> "," <> bob <> ")")
      @?= Left (errMsg TxConditionParseError "Failed reading: Impossible threshold at char 96")
      
  ]


dslSerializerTests :: TestTree
dslSerializerTests = testGroup "CryptoConditions DSL Serializer"
  [ 
    testCase "simple" $
      serializeDSL (Threshold 2 [ed2Alice, ed2Bob, ed2Bob])
      @?= ("(2 of %0, %1, %1)", [PK pkAlice, PK pkBob])
  ]


httpClientTest :: TestTree
httpClientTest = testCase "http client tests" $ do
  a <- randomIO :: IO Int
  let createSpec = object [ "creator" .= alice
                          , "outputs" .= [["1", alice]]
                          , "asset" .= object [ "msg" .= a ]
                          ]
      server = "http://localhost:9984/" :: String
  (Right stx) <- runExceptT $ do
    tx <- createTx createSpec
    API.signTx $ object [ "tx" .= tx, "key" .= aliceSec ]
  res <- runExceptT $
    httpPostTransaction $ object ["server" .= server, "tx" .= stx ]
  res @?= Right stx


alice, bob, aliceSec, bobSec :: Text
alice = "AiFE9ZaMN8xnWq1b7cSMqKFjA9xg6hWhgphsgtMwmore"
bob = "8yBU23eXXrYcLMDaaP3ke7agqF1isE8gLRjA8NmPEciB"
aliceSec = "HtjVPKhduXLFa3sb4dLsu7X47h2Ay7zFzcfaGsdj1MbB"
bobSec = "GaSipiHuPy912PUWtnahQ4Pr8Ao8c9ppyjgs6cy21Pum"


ed2Alice, ed2Bob :: CryptoCondition
ed2Alice = ed25519Condition pkAlice
ed2Bob = ed25519Condition pkBob


pkAlice, pkBob :: Ed2.PublicKey
[PK pkAlice, PK pkBob] = unsafeParseKey <$> [alice, bob]


skAlice, skBob :: SecretKey
[skAlice, skBob] = unsafeParseKey <$> [alice, bob]
