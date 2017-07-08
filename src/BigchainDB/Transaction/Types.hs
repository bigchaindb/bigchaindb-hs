{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--------------------------------------------------------------------------------
-- Transaction data types and JSON instances
--
-- TODO: This module should try not to allow instantiation of invalid
-- transactions, at least from JSON.
--------------------------------------------------------------------------------

module BigchainDB.Transaction.Types
  ( Amount
  , Asset(..)
  , Condition(..)
  , ConditionDetails(..)
  , Input(..)
  , Inputs(..)
  , Operation(..)
  , Output(..)
  , OutputLink(..)
  , OutputSpec
  , Transaction(..)
  , Txid
  , Metadata
  , AssetData
  , getTxid
  , emptyObject
  , encodeDeterm
  , removeSigs
  , nullOutputLink
  , txidAndJson
  ) where

import Data.Aeson.Quick hiding (emptyObject)
import Data.Aeson.Encode.Pretty
import Data.Aeson.Types hiding (emptyObject)
import Data.ByteString.Lazy (toStrict)
import Data.Char (isHexDigit)
import qualified Data.Text as T
import Data.Text.Read

import Lens.Micro
import Lens.Micro.Aeson

import BigchainDB.Crypto
import BigchainDB.CryptoConditions
import BigchainDB.Prelude


--------------------------------------------------------------------------------
-- Transaction ID
--
newtype Txid = Txid T.Text
  deriving (Eq, Ord, Show)


instance ToJSON Txid where
    toJSON (Txid s) = toJSON s


instance FromJSON Txid where
  parseJSON val = do
    txt <- parseJSON val
    case (T.all isHexDigit txt, T.length txt) of
      (True, 64) -> pure $ Txid txt
      _          -> fail "Malformed ID"


--------------------------------------------------------------------------------
-- Operation
--
data Operation = CREATE | TRANSFER
  deriving (Show, Ord, Eq)


instance FromJSON Operation where
  parseJSON (String "CREATE") = pure CREATE
  parseJSON (String "TRANSFER") = pure TRANSFER
  parseJSON op = fail $ "Invalid operation: " ++ show op


--------------------------------------------------------------------------------
-- Transaction
--
data Transaction = Tx
  { asset :: Asset
  , inputs :: Inputs
  , outputs :: [Output]
  , metadata :: Metadata
  } deriving (Eq, Show)


instance FromJSON Transaction where
  parseJSON = withObject "transaction" $ \obj -> do
    op <- obj .: "operation"
    tx <- Tx <$> (obj .: "asset" >>= parseAsset op)
             <*> (obj .: "inputs" >>= parseInputs op)
             <*> obj .: "outputs"
             <*> obj .: "metadata"

    -- Verify txid
    let (Txid calcTxid) = fst (txidAndJson tx)
    notEqual <- (/=Txid calcTxid) <$> obj .: "id"
    when notEqual $ fail ("Txid mismatch: " ++ T.unpack calcTxid)

    -- All done
    pure tx


instance ToJSON Transaction where
  toJSON = snd . txidAndJson


instance Ord Transaction where
  tx <= tx2 = fst (txidAndJson tx) <= fst (txidAndJson tx2)

    
txidAndJson :: Transaction -> (Txid, Value)
txidAndJson (Tx asset inputs outputs metadata) =
  let common = assetPairs asset ++
               [ "inputs" .= inputs
               , "outputs" .= outputs
               , "metadata" .= metadata
               , "version" .= String "1.0"
               ]
      noSigsTx = removeSigs $ object common
      txid = Txid $ T.pack $ sha3 $ encodeDeterm noSigsTx
      tx = object (("id" .= txid) : common)
   in (txid, tx)


getTxid :: Transaction -> Txid
getTxid = fst . txidAndJson


encodeDeterm :: ToJSON v => v -> ByteString
encodeDeterm =
  let determ = defConfig {confCompare = compare, confIndent = Spaces 0}
   in toStrict . encodePretty' determ


removeSigs :: Value -> Value
removeSigs val = build "{inputs:[{fulfillment}]}" val nulls
  where
    -- It's neccesary to apply a workaround here; the Data.Aeson.Quick.build
    -- function tries to convert our Nulls array to a Vector, which is strict
    -- in it's length. For this reason we can't use an unbounded list, which
    -- would be the natural thing to do, we just create a really long Array of
    -- null values. It's only allocated once.
    nulls = toJSON $ take 10000 $ repeat Null


--------------------------------------------------------------------------------
-- NonEmptyObject
--
-- Either Null or a non empty object
--
newtype NonEmptyObject = NEO (Maybe Object)
  deriving (Eq, Show)

instance ToJSON NonEmptyObject where
  toJSON (NEO o) = toJSON o

instance FromJSON NonEmptyObject where
  parseJSON Null = pure emptyObject
  parseJSON (Object o) =
    if o == mempty then fail "may not be empty (use null)"
                   else pure $ NEO (Just o)

emptyObject = NEO Nothing

type Metadata = NonEmptyObject
type AssetData = NonEmptyObject


--------------------------------------------------------------------------------
-- Asset
--
data Asset = Create AssetData | Transfer Txid
  deriving (Eq, Show)


instance ToJSON Asset where
  toJSON (Transfer txid) = object ["id" .= txid]
  toJSON (Create obj) = object ["data" .= obj]


parseAsset :: Operation -> Object -> Parser Asset
parseAsset CREATE val = Create <$> val .: "data"
parseAsset TRANSFER val = Transfer <$> val .: "id"


assetPairs :: Asset -> [Pair]
assetPairs a@(Create assetData) = [ "operation" .= String "CREATE", "asset" .= a ]
assetPairs a@(Transfer assetId) = [ "operation" .= String "TRANSFER", "asset" .= a ]

--------------------------------------------------------------------------------
-- Inputs
--
data Inputs = Signed [Input T.Text] | Unsigned [Input ConditionDetails]
  deriving (Eq, Show)


instance ToJSON Inputs where
  toJSON (Signed inputs) = toJSON inputs
  toJSON (Unsigned inputs) = toJSON inputs


parseInputs :: Operation -> [Object] -> Parser Inputs
parseInputs op objs = (Signed <$> safeInputs op objs)
                  <|> (Unsigned <$> safeInputs op objs)


-- | Parse inputs depending on transaction operation
safeInputs :: FromJSON f => Operation -> [Object] -> Parser [Input f]
safeInputs _ [] = fail "Transaction must have inputs"
safeInputs TRANSFER objs = mapM (parseJSON . Object) objs
safeInputs CREATE [obj] = do
  fulfillsVal <- obj .: "fulfills"
  let ffills | fulfillsVal == Null = pure nullOutputLink
             | otherwise = fail "CREATE tx does not link to an output"
  input <- Input <$> ffills <*> (obj .: "owners_before")
                            <*> (obj .: "fulfillment")
  pure [input]
safeInputs CREATE _ = fail "CREATE tx has exactly one input"


--------------------------------------------------------------------------------
-- Input with polymorphic fulfillment type (signed or unsigned)
--
data Input ffill = Input OutputLink OwnersBefore ffill
  deriving (Eq, Show)


instance (ToJSON ffill) => ToJSON (Input ffill) where
  toJSON (Input f ob ff) =
    object ["fulfills" .= f, "fulfillment" .= ff, "owners_before" .= ob]


instance (FromJSON ffill) => FromJSON (Input ffill) where
  parseJSON = withObject "input" $ \obj ->
    Input <$> (obj .: "fulfills")
          <*> (obj .: "owners_before")
          <*> (obj .: "fulfillment")


type OwnersBefore = [PublicKey]


--------------------------------------------------------------------------------
-- Amount
--
newtype Amount = Amount Word
  deriving (Eq, Num, Show)


instance ToJSON Amount where
  toJSON (Amount n) = toJSON $ show n


instance FromJSON Amount where
  parseJSON val = do
    txt <- parseJSON val
    case decimal txt of
      Right (n, "") -> pure $ Amount n
      _            -> fail ("Invalid amount: " ++ show txt)


--------------------------------------------------------------------------------
-- Output
--
data Output = Output Condition Amount
  deriving (Eq, Show)


instance ToJSON Output where
  toJSON (Output (Condition c) amount) =
    object [ "condition" .= Condition c
           , "amount" .= amount
           , "public_keys" .= getConditionPubkeys c
           ]


instance FromJSON Output where
  parseJSON = withObject "output" $ \obj ->
    Output <$> (obj .: "condition")
           <*> (obj .: "amount")


type OutputSpec = (Amount, T.Text)

--------------------------------------------------------------------------------
-- Output Conditions
--
newtype Condition = Condition CryptoCondition
  deriving (Eq, Show)


instance ToJSON Condition where
  toJSON (Condition c) =
    object [ "details" .= Details c
           , "uri" .= getConditionURI c
           ]

instance FromJSON Condition where
  parseJSON = withObject "condition" $ \obj -> do
    Details c <- obj .: "details"
    pure $ Condition c


--------------------------------------------------------------------------------
-- Condition Details
--
newtype ConditionDetails = Details CryptoCondition
  deriving (Eq, Show)


instance ToJSON ConditionDetails where
  toJSON (Details c) = getConditionDetails c


instance FromJSON ConditionDetails where
  parseJSON val = Details <$> withObject "details" parseConditionDetails val


--------------------------------------------------------------------------------
-- Output Link - identifies an Output
--
data OutputLink = OutputLink Txid Int
  deriving (Eq, Ord, Show)


instance FromJSON OutputLink where
  parseJSON = withObject "fulfills" $ \obj ->
    OutputLink <$> (obj .: "transaction_id") <*> (obj .: "output_index")


instance ToJSON OutputLink where
  toJSON (OutputLink (Txid "") _) = Null
  toJSON (OutputLink (Txid txid) off) =
    object ["transaction_id" .= txid, "output_index" .= off]


nullOutputLink :: OutputLink
nullOutputLink = OutputLink (Txid "") 0
