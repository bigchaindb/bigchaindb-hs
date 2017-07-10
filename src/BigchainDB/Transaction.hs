{-# LANGUAGE OverloadedStrings #-}

module BigchainDB.Transaction 
  ( module TTE
  , mkCreateTx
  , signTx
  , signCondition
  ) where

import qualified Crypto.PubKey.Ed25519 as Ed2

import Data.Aeson

import qualified Data.ByteString.Char8 as C8

import BigchainDB.Crypto
import BigchainDB.CryptoConditions
import BigchainDB.Transaction.Types as TT
import BigchainDB.Prelude

import BigchainDB.Transaction.Common as TTE
import BigchainDB.Transaction.Transfer as TTE
import BigchainDB.Transaction.Types as TTE


mkCreateTx :: AssetData -> PublicKey -> [OutputSpec]
           -> Metadata -> Except Err Transaction
mkCreateTx _ _ [] _ = throwE $ errMsg TxCreateError "Outputs cannot be empty"
mkCreateTx assetData (PK creator) outputSpecs metadata = do
    let ffill = ed25519Condition creator
        inputs = [Input nullOutputLink ffill]
    outputs <- mapM createOutput outputSpecs
    pure $ Tx (Create assetData) inputs outputs metadata


signCondition :: C8.ByteString -> CryptoCondition -> SecretKey -> CryptoCondition
signCondition msg cond (SK sk) = 
  let pk = Ed2.toPublic sk
      sig = Ed2.sign sk pk msg
  in fulfillEd25519 pk sig cond


signInput :: SecretKey -> ByteString -> Input -> Except Err Input
signInput sk msg (Input l cond) =
  let ffill = signCondition msg cond sk
  in if conditionIsSigned ffill
        then pure $ Input l ffill
        else throwE $ errMsg TxSignMissingPrivateKeys "missing private keys"


signTx :: SecretKey -> Transaction -> Except Err Transaction
signTx sk tx@(Tx asset inputs outputs metadata) = do 
  let msg = encodeDeterm $ removeSigs $ toJSON tx
  signedInputs <- mapM (signInput sk msg) inputs
      -- TODO: test encode signedTx == signedVal
  pure $ Tx asset signedInputs outputs metadata
