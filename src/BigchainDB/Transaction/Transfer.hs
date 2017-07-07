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
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text.Read

import Lens.Micro
import Lens.Micro.Aeson

import BigchainDB.Crypto
import BigchainDB.CryptoConditions
import BigchainDB.Prelude
import BigchainDB.Transaction.Common
import BigchainDB.Transaction.Types


mkTransferTx :: Set Transaction -> Set OutputLink -> [OutputSpec] ->
                Metadata -> Except BDBError Transaction
mkTransferTx spends links outputSpecs metadata = do

  when (spends == Set.empty) $
    throwE $ txTransferError "spends cannot be empty"

  when (outputSpecs == []) $
    throwE $ txTransferError "spends cannot be empty"

  let spendsList = Set.toList spends
      inputs = Unsigned $ Set.toList spends >>= txToInputs

  outputs <- mapM createOutput outputSpecs
  assetLink <- getAssetLink spendsList
  pure $ Tx assetLink inputs outputs metadata


getAssetLink :: [Transaction] -> Except BDBError Asset
getAssetLink txs =
  let assetIds = getAssetId <$> txs
  in case nub assetIds of
       [assetId] -> pure (Transfer assetId)
       _ -> throwE $ txTransferError "Cannot consolidate transactions with different asset IDs"
  where
    getAssetId (Tx {asset=Transfer assetId}) = assetId
    getAssetId tx = fst $ txidAndJson tx


txToInputs :: Transaction -> [Input ConditionDetails]
txToInputs tx@(Tx {outputs=outputs}) =
  convertOutput (getTxid tx) <$> zip [0..] outputs


convertOutput :: Txid -> (Int, Output) -> Input ConditionDetails
convertOutput txid (idx, (Output (Condition cond) _)) =
  Input (OutputLink txid idx) (getConditionPubkeys cond) (Details cond)
