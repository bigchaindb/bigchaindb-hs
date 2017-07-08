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
    throwE $ txTransferError "outputs cannot be empty"

  let spendsList = Set.toList spends
      allInputs = spendsList >>= txToInputs
      selectedInputs = filter (isSelectedInput links . snd) allInputs
      (inputAmounts, inputs) = unzip selectedInputs

  outputs <- mapM createOutput outputSpecs

  when (sum inputAmounts /= sum [n | (Output _ n) <- outputs]) $
     throwE $ txTransferError "Input amount not equal to output amount"

  assetLink <- getAssetLink spendsList
  pure $ Tx assetLink (Unsigned inputs) outputs metadata


isSelectedInput :: Set OutputLink -> Input a -> Bool
isSelectedInput links (Input ol _ _)
  | links == mempty = True
  | otherwise = Set.member ol links


getAssetLink :: [Transaction] -> Except BDBError Asset
getAssetLink txs =
  let assetIds = getAssetId <$> txs
  in case nub assetIds of
       [assetId] -> pure (Transfer assetId)
       _ -> throwE $ txTransferError "Cannot consolidate transactions with different asset IDs"
  where
    getAssetId (Tx {asset=Transfer assetId}) = assetId
    getAssetId tx = fst $ txidAndJson tx


txToInputs :: Transaction -> [(Amount, Input ConditionDetails)]
txToInputs tx@(Tx {outputs=outputs}) =
  [(n, convertOutput (getTxid tx) (idx, cond))
    | (idx, Output (Condition cond) n) <- zip [0..] outputs]


convertOutput :: Txid -> (Int, CryptoCondition) -> Input ConditionDetails
convertOutput txid (idx, cond) =
  Input (OutputLink txid idx) (getConditionPubkeys cond) (Details cond)
