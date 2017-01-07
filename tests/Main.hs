{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit

import qualified Crypto.PubKey.Ed25519 as Ed2

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

import TestCryptoConditions


main :: IO ()
main = defaultMain $ testGroup "Tests" [ apiTests
                                       , txTests
                                       , cryptoConditionsTests
                                       , dslParserTests
                                       , dslSerializerTests
                                       ]


apiTests :: TestTree
apiTests = testGroup "Test the JSON API"
  [
     run "createTx1" "tests/api/createTx1.json" $
         createTx "{\"creator\":\"7uQSF92GR1ZVmL7wNs3MJcg5Py2sDbpwCBmWNrYVSQs1\", \"outputs\":[[\"1\",\"(1 of 7uQSF92GR1ZVmL7wNs3MJcg5Py2sDbpwCBmWNrYVSQs1)\"]]}"
  ]
  where
    diff ref new = ["colordiff", "-u", ref, new]
    run name file act =
      let out = file ++ ".out"
       in goldenVsFileDiff name diff file out (act >>= BS.writeFile out)


txTests :: TestTree
txTests = testGroup "Test Transaction ID validation"
  [
     testCase "tx-right-id-succeeds" $ do
         res <- validateTx =<< create
         res @?= "{\"result\":\"ok\"}"
  ,
     testCase "tx-wrong-id-fails" $ do
         tx <- create
         res <- validateTx $ tx & key "id" .~ badId
         res @?= "{\"error\":\"Error in $: id incorrect\"}"
  ]
  where
    badId = "FFFd1a44abcf0a18b7aec2d406c11ed0cb0bd371847145be7822c76077ca5514"
    create = createTx "{\"creator\":\"7uQSF92GR1ZVmL7wNs3MJcg5Py2sDbpwCBmWNrYVSQs1\", \"outputs\":[[\"1\",\"(1 of 7uQSF92GR1ZVmL7wNs3MJcg5Py2sDbpwCBmWNrYVSQs1)\"]]}"



dslParserTests :: TestTree
dslParserTests = testGroup "CryptoConditions DSL Parser"
  [
    testCase "ed25519-simple" $
      runExcept (parseDSL alice) @?= Right ed2Alice

  , testCase "ed25519-fail-b58" $
      runExcept (parseDSL "0uQSF92GR1ZVmL7wNs3MJcg5Py2sDbpwCBmWNrYVSQs1")
      @?= Left "Failed reading: Expected \"({num} of ...\" or ed25519 key at char 0"

  , testCase "threshold-simple" $
      runExcept (parseDSL ("(2 of " <> alice <> ", " <> bob <> " * 2)"))
      @?= Right (Threshold 2 [ed2Alice, ed2Bob, ed2Bob])
  ]


dslSerializerTests :: TestTree
dslSerializerTests = testGroup "CryptoConditions DSL Serializer"
  [ 
    testCase "simple" $
      serializeDSL (Threshold 2 [ed2Alice, ed2Bob, ed2Bob])
      @?= ("(2 of %A, %B, %B)", Map.fromList [("A", PK pkAlice), ("B", PK pkBob)])
  ]


ed2Alice, ed2Bob :: Condition
ed2Alice = ed25519Condition pkAlice
ed2Bob = ed25519Condition pkBob

alice, bob :: Text
alice = "7uQSF92GR1ZVmL7wNs3MJcg5Py2sDbpwCBmWNrYVSQs1"
bob = "DCBsceTfmZXL5d9t3enc7VPdYpPETixD12qKXs53oW6Q"

pkAlice, pkBob :: Ed2.PublicKey
pkAlice = let (PK k) = unsafeParseKey alice in k
pkBob = let (PK k) = unsafeParseKey bob in k
