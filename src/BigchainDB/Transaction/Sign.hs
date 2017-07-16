{-# LANGUAGE OverloadedStrings #-}

module BigchainDB.Transaction.Sign
  ( signCondition
  , signTxPartial
  , txIsSigned
  ) where


import qualified Crypto.PubKey.Ed25519 as Ed2

import Data.Aeson

import qualified Data.ByteString.Char8 as C8

import BigchainDB.Crypto
import BigchainDB.CryptoConditions
import BigchainDB.Prelude
import BigchainDB.Transaction.Types


signCondition :: C8.ByteString -> CryptoCondition -> SecretKey -> CryptoCondition
signCondition msg cond (SK sk) =
  let pk = Ed2.toPublic sk
      sig = Ed2.sign sk pk msg
  in fulfillEd25519 pk sig cond


signTxPartial :: Transaction -> SecretKey -> Transaction
signTxPartial tx@(Tx {_inputs=inputs}) sk =
  let msg = encodeDeterm $ removeSigs $ toJSON tx
      sign (Input l cond) = Input l $ signCondition msg cond sk
   in tx { _inputs = sign <$> inputs }


txIsSigned :: Transaction -> Bool
txIsSigned (Tx {_inputs=inputs}) =
  let inputIsSigned (Input l cond) = conditionIsSigned cond
   in all inputIsSigned inputs
