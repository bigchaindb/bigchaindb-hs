{-# LANGUAGE OverloadedStrings #-}

module BigchainDB.CryptoConditions.DSL.Serialize
  ( serializeDSL
  , deserializeDSL
  ) where

import Control.Monad.Trans.State

import Data.Attoparsec.Text as AT hiding (decimal)
import Data.Attoparsec.Text.Lazy as ATL hiding (decimal)
import Data.Char (chr)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder.Int (decimal)

import Numeric

import Interledger.CryptoConditions.Standard

import BigchainDB.Crypto
import BigchainDB.CryptoConditions.DSL.Parse
import BigchainDB.Prelude



type Locals = Map.Map SortPK TL.Text


newtype SortPK = SortPK PublicKey deriving (Eq, Show)

instance Ord SortPK where
  compare a b = compare (show a) (show b)


serializeDSL :: Condition -> (TL.Text, Map.Map TL.Text PublicKey)
serializeDSL cond =
  let (expr, locals) = runState (serialize cond) Map.empty
      flipped = (\((SortPK a),b) -> (b,a)) <$> Map.toAscList locals
   in (expr, Map.fromList flipped)


serialize :: Condition -> State Locals TL.Text
serialize (Threshold t subs) = do
  subs' <- mapM serialize subs
  return $ "(" <> toLazyText (decimal t) <> " of "
               <> TL.intercalate ", " subs' <> ")"
serialize (Ed25519 pk _) = do
  local <- state $ localName pk
  return $ "%" <> local


localName :: PublicKey -> Locals -> (TL.Text, Locals)
localName pk locals =
  let nlocals = Map.size locals
      nextLocal = TL.pack $ showIntAtBase 26 (chr . (+65)) nlocals ""
      spk = SortPK pk
   in case Map.lookup spk locals of
       Just k -> (k, locals)
       Nothing -> (nextLocal, Map.insert spk nextLocal locals)


--------------------------------------------------------------------------------
-- Deserialize DSL - splices variables back into expression
--
deserializeDSL :: T.Text -> Map.Map T.Text T.Text -> Except String Condition
deserializeDSL expr locals = do
  spliced <- ExceptT $ return $ AT.parseOnly (spliceVars locals) expr
  parseDSL spliced


spliceVars :: Map.Map T.Text T.Text -> Parser T.Text
spliceVars locals = do
  head' <- ATL.takeWhile (/='%')
  next <- ("%" >> pure splice) <|> pure (pure "")
  (head' <>) <$> next
  where
    splice = do
      name <- takeWhile1 (\c -> c >= 'A' && c <= 'Z')
      case Map.lookup name locals of
           Just key -> (key <>) <$> spliceVars locals
           Nothing -> fail $ "%" ++ T.unpack name ++ " is not defined"
