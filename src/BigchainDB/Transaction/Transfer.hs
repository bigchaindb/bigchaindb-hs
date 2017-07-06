{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
-- Make TRANSFER transactions.
--------------------------------------------------------------------------------

module BigchainDB.Transaction.Transfer
  ( mkTransferTx
  ) where

import Data.Aeson.Quick hiding (emptyObject)
import Data.Aeson.Encode.Pretty
import Data.Aeson.Types hiding (emptyObject)
import Data.ByteString.Lazy (toStrict)
import Data.Char (isHexDigit)
import Data.List (nub)
import qualified Data.Text as T
import Data.Text.Read

import Lens.Micro
import Lens.Micro.Aeson

import BigchainDB.Crypto
import BigchainDB.CryptoConditions
import BigchainDB.Prelude
import BigchainDB.Transaction.Common
import BigchainDB.Transaction.Types


mkTransferTx :: [AnyTransaction] -> [OutputLink] -> [OutputSpec] ->
                Metadata -> Except BDBError UnsignedTransaction
mkTransferTx [] _ _ _ = throwE $ txTransferError "spends cannot be empty"
mkTransferTx _ _ [] _ = throwE $ txTransferError "outputs cannot be empty"
mkTransferTx spends links outputSpecs metadata = do
  let inputs = spends >>= txToInputs
  outputs <- mapM createOutput outputSpecs
  assetLink <- getAssetLink spends
  let tx = Tx Transfer assetLink inputs outputs metadata
      (txid, jsonVal) = txidAndJson tx
  return $ UnsignedTx txid jsonVal tx


getAssetLink :: [AnyTransaction] -> Except BDBError Asset
getAssetLink txs =
  let assetIds = (getAssetId . getTx) <$> txs
  in case nub assetIds of
       [assetId] -> pure (AssetLink assetId)
       _ -> throwE $ txTransferError "Cannot consolidate transactions with different asset IDs"
  where
    getAssetId (_, (AssetLink txid)) = txid
    getAssetId (txid, _) = txid
    getTx (AnyS (SignedTx txid _ (Tx _ a _ _ _))) = (txid, a)
    getTx (AnyU (UnsignedTx txid _ (Tx _ a _ _ _))) = (txid, a)


txToInputs :: AnyTransaction -> [Input ConditionDetails]
txToInputs (AnyS (SignedTx txid _ (Tx _ _ _ outputs _))) =
  convertOutput txid <$> zip [0..] outputs
txToInputs (AnyU (UnsignedTx txid _ (Tx _ _ _ outputs _))) =
  convertOutput txid <$> zip [0..] outputs


convertOutput :: Txid -> (Int, Output) -> Input ConditionDetails
convertOutput txid (idx, (Output (Condition cond) _)) =
  Input (OutputLink txid idx) (getConditionPubkeys cond) (Details cond)
