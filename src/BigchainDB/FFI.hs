{-# LANGUAGE ForeignFunctionInterface #-}

module BigchainDB.FFI where

import qualified Data.ByteString as BS

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

import qualified BigchainDB.API as API


foreign export ccall createTx :: FFIMethod
foreign export ccall generateKeyPair :: FFIMethod
foreign export ccall validateTx :: FFIMethod
foreign export ccall signTx :: FFIMethod
foreign export ccall parseConditionDSL :: FFIMethod
foreign export ccall signFulfillmentSpec :: FFIMethod
foreign export ccall verifyFulfillment :: FFIMethod
foreign export ccall readFulfillment :: FFIMethod


-- An FFI method takes a request string and a callback function to be
-- called with the result. The reason for the callback is so that
-- the result string pointer can be freed afterwards.
type FFIMethod = CString -> FunPtr (CString -> IO CLLong) -> IO CLLong


generateKeyPair :: FFIMethod
generateKeyPair = toFFI API.generateKeyPair


createTx :: FFIMethod
createTx = toFFI API.createTx


validateTx :: FFIMethod
validateTx = toFFI API.validateTx


signTx :: FFIMethod
signTx = toFFI API.signTx


parseConditionDSL :: FFIMethod
parseConditionDSL = toFFI API.parseConditionDSL


signFulfillmentSpec :: FFIMethod
signFulfillmentSpec = toFFI API.signFulfillmentSpec


verifyFulfillment :: FFIMethod
verifyFulfillment = toFFI API.verifyFulfillment


readFulfillment :: FFIMethod
readFulfillment = toFFI API.readFulfillment


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
