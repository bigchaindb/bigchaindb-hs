
module BigchainDB.Transaction.Common where

import BigchainDB.CryptoConditions
import BigchainDB.Prelude
import BigchainDB.Transaction.Types

createOutput :: OutputSpec -> Except BDBError Output
createOutput (amount, expr) = do
  c <- parseDSL expr
  return $ Output (Condition c) amount
