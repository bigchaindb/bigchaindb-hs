{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module BigchainDB.Crypto.B58Keys
  ( B58ED2Key(..)
  , PublicKey(..)
  , SecretKey(..)
  , Signature(..)
  , fromData
  , toData
  , parseKey
  , unsafeParseKey
  ) where

import Control.Monad.Trans.Except
import Crypto.Error (CryptoFailable(..))
import qualified Crypto.PubKey.Ed25519 as Ed2

import Data.Aeson 
import Data.Aeson.Types
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Base58
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)


class BA.ByteArrayAccess a => B58ED2Key a where
  mkKey :: BS.ByteString -> CryptoFailable a


instance B58ED2Key PublicKey where
  mkKey = fmap PublicKey . Ed2.publicKey


instance B58ED2Key SecretKey where
  mkKey = fmap SecretKey . Ed2.secretKey


instance B58ED2Key Signature where
  mkKey = fmap Signature . Ed2.signature


newtype PublicKey = PublicKey Ed2.PublicKey
  deriving (BA.ByteArrayAccess, Eq)

instance ToJSON PublicKey where
  toJSON = toJSON . b58 . BS.pack . BA.unpack

instance FromJSON PublicKey where
  parseJSON = jsonKey parseKey

instance Show PublicKey where
  show = show . toJSON

newtype SecretKey = SecretKey Ed2.SecretKey
  deriving (BA.ByteArrayAccess, Eq)

instance ToJSON SecretKey where
  toJSON = toJSON . b58 . BS.pack . BA.unpack

instance FromJSON SecretKey where
  parseJSON = jsonKey parseKey

instance Show SecretKey where
  show = show . toJSON

newtype Signature = Signature Ed2.Signature
  deriving (BA.ByteArrayAccess, Eq)

instance ToJSON Signature where
  toJSON = toJSON . b58 . BS.pack . BA.unpack

instance FromJSON Signature where
  parseJSON = jsonKey parseKey

instance Show Signature where
  show = show . toJSON


jsonKey :: (T.Text -> Except String a) -> Value -> Parser a
jsonKey f val = parseJSON val >>= either fail return . runExcept . f


parseKey :: B58ED2Key k => T.Text -> Except String k
parseKey t = do
  let mbs = decodeBase58 bitcoinAlphabet $ encodeUtf8 t
  maybe (throwE "Invalid base58 key") fromData mbs


unsafeParseKey :: B58ED2Key k => T.Text -> k
unsafeParseKey = either error id . runExcept . parseKey


fromData :: B58ED2Key k => BS.ByteString -> Except String k
fromData bs = case mkKey bs of
   CryptoPassed a -> return a
   CryptoFailed e -> throwE $ show e


toData :: B58ED2Key k => k -> BS.ByteString
toData = BS.pack . BA.unpack


b58 :: BS.ByteString -> String
b58 = BS8.unpack . encodeBase58 bitcoinAlphabet
