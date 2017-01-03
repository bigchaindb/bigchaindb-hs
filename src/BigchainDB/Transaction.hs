{-# LANGUAGE OverloadedStrings #-}

module BigchainDB.Transaction 
  ( module TT
  , mkCreateTx
  , signTx
  , createOutput
  ) where

import Data.Aeson

import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T

import Lens.Micro
import Lens.Micro.Aeson

import BigchainDB.Crypto
import BigchainDB.CryptoConditions
import BigchainDB.Transaction.Types as TT
import BigchainDB.Prelude


mkCreateTx :: Object -> PublicKey -> [(Amount, T.Text)]
           -> Object -> Except String UnsignedTransaction
mkCreateTx assetData creator outputSpecs metadata = do
    let ffill = FFTemplate $ ed25519Condition creator
        asset = AssetDefinition assetData
    when (null outputSpecs) $ throwE "mkCreateTx: outputs cannot be empty"
    outputs <- mapM createOutput outputSpecs
    return $ UnsignedTx $
      Tx Create asset [Input nullOutputLink ffill] outputs metadata


signInput :: SecretKey -> TxId -> Input FulfillmentTemplate
          -> Except String (Input T.Text)
signInput sk txid (Input l (FFTemplate cond)) =
  let pk = toPublic sk
      sig = sign sk pk (C8.pack $ show (l, txid))
      fcond = fulfillEd25519 pk sig cond
      mff = getFulfillmentB64 fcond
   in maybe (throwE "Could not sign tx") (return . Input l) mff


signTx :: UnsignedTransaction -> SecretKey -> Except String SignedTransaction
signTx u@(UnsignedTx (Tx op asset inputs outputs metadata)) sk = do
  let (txid, jsonVal) = txidAndJson u
  signedInputs <- mapM (signInput sk txid) inputs
  let signedTx = Tx op asset signedInputs outputs metadata
      signedVal = set (key "inputs") (toJSON signedInputs) jsonVal
      -- TODO: test encode signedTx == signedVal
  return $ SignedTx txid signedVal signedTx


createOutput :: (Amount, T.Text) -> Except String Output
createOutput (amount, expr) = do
  c <- parseDSL expr
  return $ Output (FFTemplate c) amount

