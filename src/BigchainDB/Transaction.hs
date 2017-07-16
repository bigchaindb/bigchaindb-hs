{-# LANGUAGE OverloadedStrings #-}

module BigchainDB.Transaction 
  ( module TTE
  , mkCreateTx
  ) where

import qualified Crypto.PubKey.Ed25519 as Ed2

import Data.Aeson

import qualified Data.ByteString.Char8 as C8

import BigchainDB.Crypto
import BigchainDB.CryptoConditions
import BigchainDB.Prelude
import BigchainDB.Transaction.Sign as TTE
import BigchainDB.Transaction.Transfer as TTE
import BigchainDB.Transaction.Types as TTE


mkCreateTx :: AssetData -> PublicKey -> [OutputSpec]
           -> Metadata -> Except Err Transaction
mkCreateTx _ _ [] _ = throwE $ errMsg InvalidParams "Outputs cannot be empty"
mkCreateTx assetData (PK creator) outputSpecs metadata = do
    let ffill = ed25519Condition creator
        inputs = [Input nullOutputLink ffill]
    outputs <- mapM createOutput outputSpecs
    pure $ Tx (Create assetData) inputs outputs metadata
