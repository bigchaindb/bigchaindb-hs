{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module TestTransaction where

import qualified Data.Text as T

import Test.SmallCheck.Series

import BigchainDB.Crypto
import BigchainDB.Transaction


instance Monad m => Serial m Operation where
  series = cons0 Create \/ cons0 Transfer


instance Monad m => Serial m Asset where
  series = cons1 AssetDefinition \/ cons1 AssetLink


instance Serial m f => Serial m (Transaction f) where
    series = Tx <$> series <*> series <*> series <*> series <*> series


