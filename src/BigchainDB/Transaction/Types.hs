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
  , Input(..)
  , Operation
  , Output(..)
  , OutputLink(..)
  , OutputSpec
  , Transaction(..)
  , Txid(..)
  , Metadata
  , AssetData
  , getTxid
  , encodeDeterm
  , removeSigs
  , nullPayload
  , nullOutputLink
  , txidAndJson
  ) where

import Data.Aeson.Quick (build)
import Data.Aeson.Encode.Pretty
import Data.Aeson.Types
import Data.ByteString.Lazy (toStrict)
import Data.Char (isHexDigit)
import qualified Data.Text as T
import Data.Text.Read

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
-- Transaction
--
data Transaction = Tx
  { _asset :: Asset
  , _inputs :: [Input]
  , _outputs :: [Output]
  , _metadata :: Metadata
  } deriving (Eq, Show)


instance FromJSON Transaction where
  parseJSON = withStrictObject "transaction" $ \obj -> do

    -- Construct tx
    op <- obj .:- "operation"
    tx <- Tx <$> (obj .:- "asset" >>= parseAsset op)
             <*> (obj .:- "inputs" >>= parseInputs op)
             <*> obj .:- "outputs"
             <*> obj .:- "metadata"

    -- Verify txid
    let (Txid calcTxid) = fst (txidAndJson tx)
    notEqual <- (/=Txid calcTxid) <$> obj .:- "id"
    when notEqual $ fail ("expected txid: " ++ T.unpack calcTxid)

    -- Verify version
    version <- obj .:- "version"
    when (version /= "1.0") $
      fail $ "Unsupported transaction version: " ++ version

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


type Operation = T.Text

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
  parseJSON Null = pure nullPayload
  parseJSON (Object o) =
    if o == mempty then fail "may not be empty (use null)"
                   else pure $ NEO (Just o)
  parseJSON val = typeMismatch "null or object" val

nullPayload :: NonEmptyObject
nullPayload = NEO Nothing

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


parseAsset :: Operation -> Value -> Parser Asset
parseAsset op = withStrictObject "asset" $ \obj ->
  case op of "CREATE" -> Create <$> obj .:- "data"
             "TRANSFER" -> Transfer <$> obj .:- "id"
             _        -> invalidOperation


assetPairs :: Asset -> [Pair]
assetPairs a@(Create _) = [ "operation" .= String "CREATE", "asset" .= a ]
assetPairs a@(Transfer _) = [ "operation" .= String "TRANSFER", "asset" .= a ]


invalidOperation :: Parser a
invalidOperation = fail "Invalid operation, expected CREATE or TRANSFER"


--------------------------------------------------------------------------------
-- Input with polymorphic fulfillment type (signed or unsigned)
--
data Input = Input OutputLink CryptoCondition
  deriving (Eq, Show)


instance ToJSON Input where
  toJSON (Input link cond) =
    let ff = maybe (getConditionDetails cond) toJSON $ getFulfillmentBase64 cond
    in object [ "fulfills" .= link
              , "fulfillment" .= ff
              , "owners_before" .= getConditionPubkeys cond
              ]


-- | Parse inputs depending on transaction operation
parseInputs :: Operation -> [Value] -> Parser [Input]
parseInputs _ [] = fail "Transaction must have inputs"
parseInputs "TRANSFER" val = mapM parseTransferInput val
parseInputs "CREATE" [val] = (:[]) <$> parseCreateInput val
parseInputs "CREATE" _ = fail "CREATE tx has exactly one input"
parseInputs _ _ = invalidOperation


parseInput :: StrictObject -> OutputLink -> Parser Input
parseInput obj link = do
  -- TODO: Verify this
  _ <- obj .:- "owners_before" :: Parser (Set (T.Text))
  let fulfillment = obj .:- "fulfillment" >>= parsePolyFulfillment
  Input link <$> fulfillment


parseTransferInput :: Value -> Parser Input
parseTransferInput = withStrictObject "input" $ \obj ->
    obj .:- "fulfills" >>= parseInput obj


parseCreateInput :: Value -> Parser Input
parseCreateInput = withStrictObject "input" $ \obj -> do
    fulfillsVal <- obj .:- "fulfills"
    fulfills <- if fulfillsVal == Null
                  then pure nullOutputLink
                  else fail "CREATE tx does not link to an output"
    parseInput obj fulfills


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
  parseJSON = withStrictObject "output" $ \obj -> do
    _ <- obj .:- "public_keys" :: Parser [PublicKey]
    Output <$> (obj .:- "condition")
           <*> (obj .:- "amount")



type OutputSpec = (Amount, T.Text)

--------------------------------------------------------------------------------
-- Output Conditions
--
newtype Condition = Condition CryptoCondition
  deriving (Eq, Show)


instance ToJSON Condition where
  toJSON (Condition c) =
    object [ "details" .= getConditionDetails c
           , "uri" .= getConditionURI c
           ]

instance FromJSON Condition where
  parseJSON = withStrictObject "condition" $ \obj -> do
    cc <- obj .:- "details" >>= parseConditionDetails
    actualUri <- obj .:- "uri" :: Parser T.Text
    let expectedUri = getConditionURI cc
    when (expectedUri /= actualUri) $
      fail $ "Expected URI: " ++ show expectedUri
    pure $ Condition cc


--------------------------------------------------------------------------------
-- Output Link - identifies an Output
--
data OutputLink = OutputLink Txid Int
  deriving (Eq, Ord, Show)


instance FromJSON OutputLink where
  parseJSON = withStrictObject "fulfills" $ \obj ->
    OutputLink <$> (obj .:- "transaction_id") <*> (obj .:- "output_index")


instance ToJSON OutputLink where
  toJSON (OutputLink (Txid "") _) = Null
  toJSON (OutputLink (Txid txid) off) =
    object ["transaction_id" .= txid, "output_index" .= off]


nullOutputLink :: OutputLink
nullOutputLink = OutputLink (Txid "") 0
