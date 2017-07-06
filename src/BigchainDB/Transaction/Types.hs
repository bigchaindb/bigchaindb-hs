{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
-- Transaction data types and JSON instances
--
-- TODO: This module should try not to allow instantiation of invalid
-- transactions, at least from JSON.
--------------------------------------------------------------------------------

module BigchainDB.Transaction.Types
  ( Amount
  , AnyTransaction(..)
  , SignedTransaction(..)
  , UnsignedTransaction(..)
  , Asset(..)
  , Condition(..)
  , ConditionDetails(..)
  , Input(..)
  , NonEmptyObject
  , Operation(..)
  , Output(..)
  , Transaction(..)
  , Txid
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
  deriving (Eq, Show)


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
data Operation = Create | Transfer
  deriving (Show, Eq)


instance ToJSON Operation where
  toJSON Create = String "CREATE"
  toJSON Transfer = String "TRANSFER"


instance FromJSON Operation where
  parseJSON (String "CREATE") = pure Create
  parseJSON (String "TRANSFER") = pure Transfer
  parseJSON _ = fail "Invalid operation"


--------------------------------------------------------------------------------
-- Transaction
--
-- Has no fromJSON instance, use AnyTransaction
--
-- TODO: Any way to type create vs transfer?
--
data Transaction f =
  Tx Operation Asset [Input f] [Output] NonEmptyObject
  deriving (Show)


parseTx :: FromJSON f => Value -> Parser (Transaction f)
parseTx = withObject "transaction" $ \obj -> do
    op <- obj .: "operation"
    Tx <$> pure op
       <*> (parseAsset op =<< obj .: "asset")
       <*> (parseInputs op =<< obj .: "inputs")
       <*> obj .: "outputs"
       <*> obj .: "metadata"


txidAndJson :: ToJSON f => Transaction f -> (Txid, Value)
txidAndJson (Tx op asset inputs outputs metadata) =
  let tx = object (("id" .= txid) : ("inputs" .= toJSON inputs) : common)
      common = [ "operation" .= op
               , "outputs" .= outputs
               , "metadata" .= metadata
               , "asset" .= asset
               , "version" .= String "1.0"
               ]
      inputsNoSigs = (\(Input f ob _) -> Input f ob Null) <$> inputs
      txNoSigs = object (("inputs" .= inputsNoSigs) : common)
      txid = Txid $ T.pack $ sha3 $ encodeDeterm txNoSigs
   in (txid, tx)


encodeDeterm :: ToJSON v => v -> ByteString
encodeDeterm =
  let determ = defConfig {confCompare = compare, confIndent = Spaces 0}
   in toStrict . encodePretty' determ


removeSigs :: Value -> Value -- TODO: THIS IS BROKEN
removeSigs val = build "{inputs:[{fulfillment}]}" val $ [Null] -- repeat Null

--------------------------------------------------------------------------------
--

data AnyTransaction = AnyS SignedTransaction
                    | AnyU UnsignedTransaction
  deriving (Show)


instance FromJSON AnyTransaction where
  parseJSON value = do
    let mFfill = value ^? key "inputs" . nth 0 . key "fulfillment"
    case mFfill of
         (Just (String _)) -> AnyS <$> parseJSON value
         _                 -> AnyU <$> parseJSON value


--------------------------------------------------------------------------------
-- Signed transaction
--
-- Represents a signed transaction, having an ID, a JSON representation
-- and having string fulfillments
--
data SignedTransaction =
  SignedTx Txid Value (Transaction T.Text)
  deriving (Show)


instance FromJSON SignedTransaction where
  parseJSON val = do
    txid <- (.: "id") =<< parseJSON val
    tx <- parseTx val
    let (txid', value) = txidAndJson tx
    when (txid /= txid') $ fail "Txid mismatch"
    --parseSignatures tx TODO
    pure $ SignedTx txid value tx


instance ToJSON SignedTransaction where
  toJSON (SignedTx _ val _) = val


--------------------------------------------------------------------------------
-- Unsigned transaction
--
-- Represents an unsigned transaction, having an ID, a JSON representation
-- and having template fulfillments
--
data UnsignedTransaction =
  UnsignedTx Txid Value (Transaction ConditionDetails)
  deriving (Show)


instance FromJSON UnsignedTransaction where
  parseJSON val = do
    mtxid <- (.:?"id") =<< parseJSON val
    tx <- parseTx val
    let (txid', value) = txidAndJson tx
    when (fmap (==txid') mtxid == Just False) $ fail "Txid mismatch"
    pure $ UnsignedTx txid' value tx


instance ToJSON UnsignedTransaction where
  toJSON (UnsignedTx _ val _) = val


--------------------------------------------------------------------------------
-- NonEmptyObject
--
-- Either Null or a non empty object
--
newtype NonEmptyObject = NEO (Maybe Object)
  deriving (Show)

instance ToJSON NonEmptyObject where
  toJSON (NEO o) = toJSON o

instance FromJSON NonEmptyObject where
  parseJSON Null = pure emptyObject
  parseJSON (Object o) =
    if o == mempty then fail "may not be empty (use null)"
                   else pure $ NEO (Just o)

emptyObject = NEO Nothing

--------------------------------------------------------------------------------
-- Asset
--
data Asset = AssetDefinition NonEmptyObject | AssetLink Txid
  deriving (Show)


instance ToJSON Asset where
  toJSON (AssetLink txid) = object ["id" .= txid]
  toJSON (AssetDefinition obj) = object ["data" .= obj]


parseAsset :: Operation -> Object -> Parser Asset
parseAsset Create val = AssetDefinition <$> val .: "data"
parseAsset Transfer val = AssetLink <$> val .: "id"


--------------------------------------------------------------------------------
-- Input with polymorphic fulfillment type (signed or unsigned)
--
data Input ffill = Input OutputLink OwnersBefore ffill
  deriving (Show)


instance (ToJSON ffill) => ToJSON (Input ffill) where
  toJSON (Input f ob ff) =
    object ["fulfills" .= f, "fulfillment" .= ff, "owners_before" .= ob]


instance (FromJSON ffill) => FromJSON (Input ffill) where
  parseJSON = withObject "input" $ \obj ->
    Input <$> (obj .: "fulfills")
          <*> (obj .: "owners_before")
          <*> (obj .: "fulfillment")


-- | Parse inputs depending on transaction operation
parseInputs :: FromJSON f => Operation -> [Object] -> Parser [Input f]
parseInputs Transfer objs = mapM (parseJSON .  Object) objs
parseInputs Create [obj] = do
  fulfillsVal <- obj .: "fulfills"
  let ffills | fulfillsVal == Null = pure nullOutputLink
             | otherwise = fail "CREATE tx does not link to an output"
  input <- Input <$> ffills <*> (obj .: "owners_before")
                            <*> (obj .: "fulfillment")
  pure [input]
parseInputs Create _ = fail "CREATE tx has exactly one input"


type OwnersBefore = [PublicKey]


--------------------------------------------------------------------------------
-- Amount
--
newtype Amount = Amount Word
  deriving (Show)


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
  deriving (Show)


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


--------------------------------------------------------------------------------
-- Output Conditions
--
newtype Condition = Condition CryptoCondition
  deriving (Show)


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
  deriving (Show)


instance ToJSON ConditionDetails where
  toJSON (Details c) = getConditionDetails c


instance FromJSON ConditionDetails where
  parseJSON val = Details <$> withObject "details" parseConditionDetails val


--------------------------------------------------------------------------------
-- Output Link - identifies an Output
--
data OutputLink = OutputLink Txid Int
  deriving (Show)


instance FromJSON OutputLink where
  parseJSON = withObject "fulfills" $ \obj ->
    OutputLink <$> (obj .: "txid") <*> (obj .: "output")


instance ToJSON OutputLink where
  toJSON (OutputLink (Txid "") _) = Null
  toJSON (OutputLink (Txid txid) off) =
    object ["txid" .= txid, "output" .= off]


nullOutputLink :: OutputLink
nullOutputLink = OutputLink (Txid "") 0
