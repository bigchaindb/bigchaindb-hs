{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
-- Make TRANSFER transactions.
--------------------------------------------------------------------------------

module BigchainDB.Transaction.Transfer
  ( mkTransferTx
  ) where

import Data.List (nub)
import qualified Data.Set as Set

import BigchainDB.CryptoConditions
import BigchainDB.Prelude
import BigchainDB.Transaction.Types


mkTransferTx :: Set Transaction -> Set OutputLink -> [OutputSpec] ->
                Metadata -> Except Err Transaction
mkTransferTx spends links outputSpecs metadata = do

  when (spends == Set.empty) $
    throwE $ errMsg InvalidParams "spends cannot be empty"

  when (outputSpecs == []) $
    throwE $ errMsg InvalidParams "outputs cannot be empty"

  let spendsList = Set.toList spends
      allInputs = spendsList >>= txToInputs
      selectedInputs = filter (isSelectedInput links . snd) allInputs
      (inputAmounts, inputs) = unzip selectedInputs

  outputs <- mapM createOutput outputSpecs

  when (sum inputAmounts /= sum [n | (Output _ n) <- outputs]) $
     throwE $ errMsg InvalidParams "Input amount not equal to output amount"

  assetLink <- getAssetLink spendsList
  pure $ Tx assetLink inputs outputs metadata


isSelectedInput :: Set OutputLink -> Input -> Bool
isSelectedInput links (Input ol _)
  | links == mempty = True
  | otherwise = Set.member ol links


getAssetLink :: [Transaction] -> Except Err Asset
getAssetLink txs =
  let assetIds = getAssetId <$> txs
  in case nub assetIds of
       [assetId] -> pure (Transfer assetId)
       _ -> throwE $ errMsg InvalidParams "Cannot consolidate transactions with different asset IDs"
  where
    getAssetId (Tx {_asset=Transfer assetId}) = assetId
    getAssetId tx = fst $ txidAndJson tx


txToInputs :: Transaction -> [(Amount, Input)]
txToInputs tx@(Tx {_outputs=outputs}) =
  [(n, convertOutput (getTxid tx) (idx, cond))
    | (idx, Output (Condition cond) n) <- zip [0..] outputs]


convertOutput :: Txid -> (Int, CryptoCondition) -> Input
convertOutput txid (idx, cond) =
  Input (OutputLink txid idx) cond
