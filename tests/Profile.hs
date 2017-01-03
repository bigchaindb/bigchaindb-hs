{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import System.Environment

import BigchainDB.API
import Data.Monoid


main :: IO ()
main = do
  let n = 10000
  [s] <- getArgs
  print =<< loop n (C8.pack s) createTx


loop :: Int -> BS.ByteString -> (BS.ByteString -> IO BS.ByteString) -> IO BS.ByteString
loop n bs act = do
  o <- act (bs <> if mod n 2 == 0 then " " else "")
  seq o $ if n == 0 then return o else loop (n-1) bs act
