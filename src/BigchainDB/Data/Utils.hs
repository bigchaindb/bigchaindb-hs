{-# LANGUAGE OverloadedStrings #-}

module BigchainDB.Data.Utils where


import qualified Data.Set as Set
import Data.Text


setToString :: Set.Set Text -> String
setToString = unpack . intercalate "," . Set.toList
