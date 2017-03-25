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
import qualified Data.Text as T

import Lens.Micro
import Lens.Micro.Aeson

import BigchainDB.Crypto
import BigchainDB.CryptoConditions
import BigchainDB.Transaction.Types as TT
import BigchainDB.Prelude

import BigchainDB.Transaction.Types as TTE
  hiding (SignedTransaction(..), UnsignedTransaction(..))


mkCreateTx :: Object -> PublicKey -> [(Amount, T.Text)]
           -> Object -> Except BDBError UnsignedTransaction
mkCreateTx assetData (PK creator) outputSpecs metadata = do
    let ffill = Condition $ ed25519Condition creator
        asset = AssetDefinition assetData
    when (null outputSpecs) $ throwE $ txCreateError "Outputs cannot be empty"
    outputs <- mapM createOutput outputSpecs
    let tx = Tx Create asset [Input nullOutputLink ffill] outputs metadata
        (txid, jsonVal) = txidAndJson tx
    return $ UnsignedTx txid jsonVal tx


signCondition :: C8.ByteString -> CryptoCondition -> SecretKey -> CryptoCondition
signCondition msg cond (SK sk) = 
  let pk = Ed2.toPublic sk
      sig = Ed2.sign sk pk msg
  in fulfillEd25519 pk sig cond


signInput :: SecretKey -> Txid -> Input Condition
          -> Except BDBError (Input T.Text)
signInput sk txid (Input l (Condition cond)) =
  let msg = C8.pack $ show (l, txid)
      fcond = signCondition msg cond sk
      mff = getFulfillmentBase64 fcond
   in maybe (throwE missingPrivateKeys) (return . Input l) mff


signTx :: SecretKey -> UnsignedTransaction -> Except BDBError SignedTransaction
signTx sk (UnsignedTx txid jsonVal tx) = do
  let (Tx op asset inputs outputs metadata) = tx
  signedInputs <- mapM (signInput sk txid) inputs
  let signedTx = Tx op asset signedInputs outputs metadata
      signedVal = set (key "inputs") (toJSON signedInputs) jsonVal
      -- TODO: test encode signedTx == signedVal
  return $ SignedTx txid signedVal signedTx


createOutput :: (Amount, T.Text) -> Except BDBError Output
createOutput (amount, expr) = do
  c <- parseDSL expr
  return $ Output (Condition c) amount
