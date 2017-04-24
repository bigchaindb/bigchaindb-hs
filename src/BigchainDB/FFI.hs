{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

module BigchainDB.FFI where

import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (toStrict)
import qualified Data.Map as Map
import Data.Text

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

import BigchainDB.API as API
import BigchainDB.Prelude


foreign export ccall jsonRPC :: FFIMethod


-- An FFI method takes a request string and a callback function to be
-- called with the result. The reason for the callback is so that
-- the result string pointer can be freed afterwards.
type FFIMethod = CString -> FunPtr (CString -> IO CLLong) -> IO CLLong


-- | FFI interface for API method
toFFI :: (BS.ByteString -> IO BS.ByteString) -> FFIMethod
toFFI method req callback = do
  bs <- BS.packCString req
  result <- method bs
  -- Optimize: useAsCStringUnsafe for zero copy
  BS.useAsCString result (mkFun callback)


-- | Not actually a foreign import but a magical function which
--   converts a dynamic foreign callback to a haskell function
foreign import ccall "dynamic" mkFun :: FunPtr (Ptr a -> IO CLLong)
                                     -> (Ptr a -> IO CLLong)


jsonRPC :: FFIMethod
jsonRPC = toFFI $ pure . jsonRPC'


jsonRPC' :: BS.ByteString -> BS.ByteString
jsonRPC' bs =
  let res = case eitherDecodeStrict bs of
                 Left err -> wrapJsonError (parseError err)
                 Right res -> runJsonRpc val
   in toStrict $ encode res
  where
    parseError = BDBError (-32700) Null
