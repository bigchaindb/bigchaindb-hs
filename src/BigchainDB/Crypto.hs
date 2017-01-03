{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module BigchainDB.Crypto
  ( module BK
  , genKeyPair
  , sha3
  , sign
  , toPublic
  , verify
  ) where

import Crypto.Error (CryptoFailable(..))
import Crypto.Hash
import Crypto.Random
import qualified Crypto.PubKey.Ed25519 as Ed2
import Data.ByteArray
import Data.ByteString (ByteString)

import BigchainDB.Crypto.B58Keys as BK


sign :: SecretKey -> PublicKey -> ByteString -> Signature
sign (SecretKey sk) (PublicKey pk) = Signature . Ed2.sign sk pk


toPublic :: SecretKey -> PublicKey
toPublic (SecretKey sk) = PublicKey $ Ed2.toPublic sk


verify :: ByteArrayAccess ba => PublicKey -> ba -> Signature -> Bool
verify (PublicKey pk) msg (Signature s) = Ed2.verify pk msg s


sha3 :: ByteString -> String
sha3 bs = show (hash bs :: Digest SHA3_256)


genKeyPair :: IO (PublicKey, SecretKey)
genKeyPair = do
  drg <- getSystemDRG
  -- TODO: scrubbed bytes array?
  let (bs,_) = randomBytesGenerate 32 drg
      (CryptoPassed sk) = Ed2.secretKey (bs::ByteString)
  return (PublicKey $ Ed2.toPublic sk, SecretKey sk)
  

