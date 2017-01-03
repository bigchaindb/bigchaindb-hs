{-# LANGUAGE ScopedTypeVariables #-}

import GHCJS.Marshal(fromJSVal)
import GHCJS.Foreign.Callback (Callback, syncCallback1')
import Data.JSString (JSString, unpack, pack)
import qualified Data.ByteString.Char8 as B8
import GHCJS.Types (JSVal, jsval)
import JavaScript.Object (Object, create, setProp)
import Data.Maybe (fromJust)

import BigchainDB.API


foreign import javascript unsafe "BigchainDB = $1"
  set_val :: Object -> IO ()

foreign import javascript unsafe
  "(function() { var f = $1; return function(s) { return JSON.parse(f(JSON.stringify(s))) } })()"
  jsonMethod :: (Callback (JSVal -> IO JSVal)) -> (Callback (JSVal -> IO JSVal))


jsMethod :: (B8.ByteString -> IO B8.ByteString)
          -> IO (Callback (JSVal -> IO JSVal))
jsMethod f = jsonMethod <$> (syncCallback1' $ \jv -> do
  input <- B8.pack . unpack . fromJust <$> fromJSVal jv
  jsval . pack . B8.unpack <$> f input)



main :: IO ()
main = do
  (o :: Object) <- create
  setMethod "generateKeyPair" generateKeyPair o
  setMethod "createTx" createTx o
  set_val o
  where
    setMethod name method o = do
      m <- jsMethod method
      setProp (pack name :: JSString) (jsval $ m) o
