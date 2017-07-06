
module BigchainDB.Exceptions where

import Control.Monad.Trans.Except

import Data.Aeson

data BDBError = BDBError Int Value String
  deriving (Eq, Show)


jerr :: Except String a -> (String -> BDBError) -> Except BDBError a
jerr = flip withExcept


invalidFulfillment :: String -> BDBError
invalidFulfillment = BDBError 102 Null


badTx :: String -> BDBError
badTx = BDBError 100 Null


conditionDslParseError :: String -> BDBError
conditionDslParseError = BDBError 101 Null 


txCreateError :: String -> BDBError
txCreateError = BDBError 110 Null


txTransferError :: String -> BDBError
txTransferError = BDBError 111 Null


missingPrivateKeys :: BDBError
missingPrivateKeys = BDBError 120 Null "Missing private keys"
