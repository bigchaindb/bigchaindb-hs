module BigchainDB.API.Utils where

import Data.Aeson.Types

import BigchainDB.Prelude


type JsonMethod = Value -> ExceptT Err IO Value


ioMethod :: Monad m => (StrictObject -> Parser (ExceptT Err m Value)) ->
                       Value -> ExceptT Err m Value
ioMethod parse val = do
  let res = parseEither (withStrictObject "object" parse) val
  join $ either (throwE . errStr InvalidParams) pure res


pureMethod :: (StrictObject -> Parser (ExceptT Err Identity Value)) -> JsonMethod
pureMethod parse val = do
  let (Identity res) = runExceptT $ ioMethod parse val
  ExceptT $ pure res
