{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module BigchainDB.Crypto
  ( module BK
  , genKeyPair
  , sha3
  , Ed2.sign
  , Ed2.toPublic
  , Ed2.verify
  ) where

import Crypto.Error (CryptoFailable(..))
import Crypto.Hash
import Crypto.Random
import qualified Crypto.PubKey.Ed25519 as Ed2
import Data.ByteString (ByteString)

import BigchainDB.Crypto.B58Keys as BK


sha3 :: ByteString -> String
sha3 bs = show (hash bs :: Digest SHA3_256)


genKeyPair :: IO (PublicKey, SecretKey)
genKeyPair = do
  drg <- getSystemDRG
  -- TODO: scrubbed bytes array?
  let (bs,_) = randomBytesGenerate 32 drg
      (CryptoPassed sk) = Ed2.secretKey (bs::ByteString)
  return (PK $ Ed2.toPublic sk, SK sk)
  

