{-# LANGUAGE OverloadedStrings #-}

module BigchainDB.Data.Aeson where


import Data.Aeson.Types
import Data.IORef
import Data.HashMap.Strict
import qualified Data.Set as Set
import Data.Text

import System.IO.Unsafe


data StrictObject = StrictObject Object (IORef (Set.Set Text))


(.:-) :: FromJSON v => StrictObject -> Text -> Parser v
(.:-) so key = addKeyLookup (.:) key so


(.:-?) :: FromJSON v => StrictObject -> Text -> Parser (Maybe v)
(.:-?) so key = addKeyLookup (.:?) key so


addKeyLookup :: FromJSON v => (Object -> Text -> Parser v) -> Text ->
                StrictObject -> Parser v
addKeyLookup op key (StrictObject obj ref) =
  let p = unsafePerformIO $ modifyIORef' ref $ Set.insert key
  in seq p $ op obj key


withStrictObject :: String -> (StrictObject -> Parser a) -> Value -> Parser a
withStrictObject label act = withObject label $ \obj -> do
  let ref = unsafePerformIO $ newIORef Set.empty
  r <- act $ StrictObject obj ref
  let keysRead = unsafePerformIO (readIORef ref)
      objKeys = Set.fromList $ keys obj
      extraKeys = Set.difference objKeys keysRead
  if extraKeys /= mempty
     then let shown = unpack $ intercalate "," $ Set.toList extraKeys
          in fail (label ++ ": extra keys: " ++ shown)
     else pure r
