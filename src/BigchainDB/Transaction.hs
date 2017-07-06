{-# LANGUAGE OverloadedStrings #-}

module BigchainDB.Transaction 
  ( module TTE
  , mkCreateTx
  , signTx
  , signCondition
  , createOutput
  ) where

import qualified Crypto.PubKey.Ed25519 as Ed2

import Data.Aeson

import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Lazy (toStrict)
import qualified Data.Text as T

import Lens.Micro
import Lens.Micro.Aeson

import BigchainDB.Crypto
import BigchainDB.CryptoConditions
import BigchainDB.Transaction.Types as TT
import BigchainDB.Prelude

import BigchainDB.Transaction.Types as TTE
  hiding (SignedTransaction(..), UnsignedTransaction(..))


mkCreateTx :: NonEmptyObject -> PublicKey -> [(Amount, T.Text)]
           -> NonEmptyObject -> Except BDBError UnsignedTransaction
mkCreateTx assetData (PK creator) outputSpecs metadata = do
    let ffill = Details $ ed25519Condition creator
        asset = AssetDefinition assetData
    when (null outputSpecs) $ throwE $ txCreateError "Outputs cannot be empty"
    outputs <- mapM createOutput outputSpecs
    let tx = Tx Create asset [Input nullOutputLink [PK creator] ffill] outputs metadata
        (txid, jsonVal) = txidAndJson tx
    return $ UnsignedTx txid jsonVal tx


signCondition :: C8.ByteString -> CryptoCondition -> SecretKey -> CryptoCondition
signCondition msg cond (SK sk) = 
  let pk = Ed2.toPublic sk
      sig = Ed2.sign sk pk msg
  in fulfillEd25519 pk sig cond


signInput :: SecretKey -> ByteString -> Input ConditionDetails
          -> Except BDBError (Input T.Text)
signInput sk msg (Input l ob (Details cond)) =
  let fcond = signCondition msg cond sk
      mff = getFulfillmentBase64 fcond
   in maybe (throwE missingPrivateKeys) (return . Input l ob) mff


signTx :: SecretKey -> UnsignedTransaction -> Except BDBError SignedTransaction
signTx sk unsigned@(UnsignedTx txid jsonVal tx) = do
  let (Tx op asset inputs outputs metadata) = tx
  let msg = encodeDeterm $ removeSigs $ toJSON unsigned
  signedInputs <- mapM (signInput sk msg) inputs
  let signedTx = Tx op asset signedInputs outputs metadata
      signedVal = set (key "inputs") (toJSON signedInputs) jsonVal
      -- TODO: test encode signedTx == signedVal
  return $ SignedTx txid signedVal signedTx


createOutput :: (Amount, T.Text) -> Except BDBError Output
createOutput (amount, expr) = do
  c <- parseDSL expr
  return $ Output (Condition c) amount
