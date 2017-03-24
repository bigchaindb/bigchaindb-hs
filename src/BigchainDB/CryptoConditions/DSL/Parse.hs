{-# LANGUAGE OverloadedStrings #-}

module BigchainDB.CryptoConditions.DSL.Parse
  ( parseDSL
  ) where


import Data.Attoparsec.Text
import qualified Data.Text as T

import BigchainDB.Crypto
import BigchainDB.CryptoConditions.Types
import BigchainDB.Prelude


parseDSL :: T.Text -> Except String Condition
parseDSL t = complete $ parse dslParser t
  where
    complete (Done _ r) = return r 
    complete (Partial f) = complete $ f ""
    complete (Fail rest _ msg) =
      let offset = T.length t - T.length rest
       in throwE $ msg <> " at char " <> show offset


dslParser :: Parser Condition
dslParser = cond <* (endOfInput <|> fail "Expected end")
  where
    ss = skipSpace
    ex s = string s <|> fail ("Expected \"" <> T.unpack s <> "\"")
    cond :: Parser Condition
    cond = ss *> 
      expect2 thresholdHead ed25519
              "Expected \"({num} of ...\" or ed25519 key"
    thresholdHead = "(" *> ss *> (thresholdRest <$> decimal) <* ss <* "of" <* ss
    thresholdRest t = Threshold t <$> inner <* ss <* ex ")" <* ss
    subcondition = flip replicate <$> cond <*> weight
    inner = concat <$> sepBy1 subcondition (ss >> "," >> ss)
    weight = (ss *> "*" *> ss *> decimal) <|> pure 1
    -- TODO: This needs to return a better error for invalid PKs
    ed25519 = do
      (t, l) <- runScanner 0 (\l c -> if isBase58 c then Just (l+1) else Nothing)
      if (l::Int) == 43 || l == 44
         then do (PK k) <- exceptToFail (parseKey t)
                 pure $ pure $ ed25519Condition k
         else fail "Not a public key"


isBase58 :: Char -> Bool
isBase58 c = (c >= 'a' && c <= 'z' && c /= 'l')
          || (c <= '9' && c >= '1')
          || (c >= 'A' && c <= 'Z' && c /= 'I' && c /= 'O')


-- | Attoparsec backtracks on every failure, which makes it hard to track the
--   source of errors. The remedy to this is to identify a branch before
--   parsing all of it, otherwise alternate branches will eat the error message.
expect2 :: Parser (Parser a) -> Parser (Parser a) -> String -> Parser a
expect2 pred1 pred2 err = do
  res <- pred1 <|> pred2 <|> fail err
  res

