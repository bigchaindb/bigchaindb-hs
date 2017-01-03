{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module BigchainDB.Crypto.B58Keys
  ( B58ED2Key(..)
  , PublicKey(..)
  , SecretKey(..)
  , Signature(..)
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


class B58ED2Key a where
  parseKey :: T.Text -> Except String a
  unsafeParseKey :: T.Text -> a
  unsafeParseKey = either error id . runExcept . parseKey


instance B58ED2Key PublicKey where
  parseKey = fmap PublicKey . parseKey' Ed2.publicKey


instance B58ED2Key SecretKey where
  parseKey = fmap SecretKey . parseKey' Ed2.secretKey


instance B58ED2Key Signature where
  parseKey = fmap Signature . parseKey' Ed2.signature


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

parseKey' :: (BS.ByteString -> CryptoFailable a) -> T.Text -> Except String a
parseKey' f t = do
  let mbs = decodeBase58 bitcoinAlphabet $ encodeUtf8 t
  bs <- maybe (throwE "Invalid base58 key") return mbs
  case f bs of
       CryptoPassed a -> return a
       CryptoFailed e -> throwE $ show e


b58 :: BS.ByteString -> String
b58 = BS8.unpack . encodeBase58 bitcoinAlphabet
