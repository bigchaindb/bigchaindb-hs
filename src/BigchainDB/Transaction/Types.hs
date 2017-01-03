{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
-- Transaction data types and JSON instances
--
-- TODO: This module should try not to allow instantiation of invalid
-- transactions. Hide constructors where appropriate.
--------------------------------------------------------------------------------

module BigchainDB.Transaction.Types
  ( Amount
  , AnyTransaction(..)
  , Asset(..)
  , FulfillmentTemplate(..)
  , Input(..)
  , Operation(..)
  , Output(..)
  , SignedTransaction(..)
  , Transaction(..)
  , TxId
  , UnsignedTransaction(..)
  , nullOutputLink
  , txidAndJson
  ) where

import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Aeson.Types
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
newtype TxId = TxId T.Text
  deriving (Show)


instance ToJSON TxId where
    toJSON (TxId s) = toJSON s


instance FromJSON TxId where
  parseJSON val = do
    txt <- parseJSON val
    case (T.all isHexDigit txt, T.length txt) of
      (True, 64) -> pure $ TxId txt
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
-- TODO: I think all transactions should have IDs, but optional in the case
-- of parsing an unsigned tx.
--
-- TODO: Any way to type create vs transfer?
--
data Transaction ffill =
  Tx Operation Asset [Input ffill] [Output] Object
  deriving (Show)


-- | Parses a transaction with a given fulfillment type
instance FromJSON ffill => FromJSON (Transaction ffill) where
  parseJSON = withObject "transaction" $ \obj -> do
    op <- obj .: "operation"
    Tx <$> pure op
       <*> (parseAsset op =<< obj .: "asset")
       <*> (parseInputs op =<< obj .: "inputs")
       <*> obj .: "outputs"
       <*> obj .: "metadata"


--------------------------------------------------------------------------------
-- Unsigned Transaction 
--
-- Represents an unsigned Transaction having no ID and having
-- templates for fulfillments
--
data UnsignedTransaction = UnsignedTx (Transaction FulfillmentTemplate)


instance ToJSON UnsignedTransaction where
  toJSON = snd . txidAndJson


txidAndJson :: UnsignedTransaction -> (TxId, Value)
txidAndJson (UnsignedTx (Tx op asset inputs outputs metadata)) =
  let tx = object (("id" .= txid) : ("inputs" .= toJSON inputs) : common)
      common = [ "operation" .= op
               , "outputs" .= outputs
               , "metadata" .= metadata
               , "asset" .= asset
               ]
      inputsNoSigs = (\(Input f _) -> Input f Null) <$> inputs
      txNoSigs = object (("inputs" .= inputsNoSigs) : common)
      txid = TxId $ T.pack $ sha3 $ toStrict $ encodePretty' determ txNoSigs
      determ = defConfig { confCompare = compare, confIndent=Spaces 0 }
   in (txid, tx)


--------------------------------------------------------------------------------
-- Signed transaction
--
-- Represents a signed transaction, having an ID, a JSON representation
-- and having string fulfillments
--
data SignedTransaction = SignedTx TxId Value (Transaction T.Text)


instance ToJSON SignedTransaction where
  toJSON (SignedTx _ val _) = val


--------------------------------------------------------------------------------
-- Either a signed or an unsigned transaction
--
data AnyTransaction = Signed SignedTransaction
                    | Unsigned UnsignedTransaction


-- | Detects and parses either a signed or unsigned transaction
instance FromJSON AnyTransaction where
  parseJSON value = do
    let mId = value ^? key "id"
        mFfill = value ^? key "inputs" . nth 0 . key "fulfillment"
    case (mId, mFfill) of
      (Just txid, Just (String _)) -> do
        stx <- SignedTx <$> parseJSON txid <*> pure value <*> parseJSON value
        return $ Signed stx
      _ -> Unsigned . UnsignedTx <$> parseJSON value


--------------------------------------------------------------------------------
-- Asset
--
data Asset = AssetDefinition Object | AssetLink TxId
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
data Input ffill = Input OutputLink ffill
  deriving (Show)


instance (ToJSON ffill) => ToJSON (Input ffill) where
  toJSON (Input f ff) =
    object ["fulfills" .= f, "fulfillment" .= ff]


instance (FromJSON ffill) => FromJSON (Input ffill) where
  parseJSON = withObject "input" $ \obj ->
    Input <$> (obj .: "fulfills") <*> (obj .: "fulfillment")


-- | Parse inputs depending on transaction operation
parseInputs :: FromJSON f => Operation -> [Object] -> Parser [Input f]
parseInputs Transfer objs = mapM (parseJSON .  Object) objs
parseInputs Create [obj] = do
  fulfillsVal <- obj .: "fulfills"
  let ffills | fulfillsVal == Null = pure nullOutputLink
             | otherwise = fail "CREATE tx does not link to an output"
  input <- Input <$> ffills <*> (obj .: "fulfillment")
  pure [input]
parseInputs Create _ = fail "CREATE tx has exactly one input"


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
data Output = Output FulfillmentTemplate Amount
  deriving (Show)


instance ToJSON Output where
  toJSON (Output cond amount) =
    object [ "condition" .= cond
           , "amount" .= amount
           ]


instance FromJSON Output where
  parseJSON = withObject "output" $ \obj ->
    Output <$> (obj .: "condition")
           <*> (obj .: "amount")


--------------------------------------------------------------------------------
-- Fulfillment Template - Details needed to sign
--
newtype FulfillmentTemplate = FFTemplate Condition
  deriving (Show)


instance ToJSON FulfillmentTemplate where
  toJSON (FFTemplate c) =
    let (expr, locals) = serializeDSL c
    in object [ "uri" .= getURI c
              , "expr" .= expr
              , "locals" .= locals
              ]


instance FromJSON FulfillmentTemplate where
  parseJSON = withObject "fulfillmentTemplate" $ \obj -> do
    econdition <- deserializeDSL <$> obj .: "expr"
                                 <*> obj .: "locals"
    cond <- exceptToFail econdition
    uri <- obj .: "uri"
    if getURI cond /= uri
       then fail "Incorrect condition URI"
       else pure $ FFTemplate cond


--------------------------------------------------------------------------------
-- Output Link - identifies an Output
--
data OutputLink = OutputLink TxId Int
  deriving (Show)


instance FromJSON OutputLink where
  parseJSON = withObject "fulfills" $ \obj ->
    OutputLink <$> (obj .: "txid") <*> (obj .: "output")


instance ToJSON OutputLink where
  toJSON (OutputLink (TxId "") _) = Null
  toJSON (OutputLink (TxId txid) off) =
    object ["txid" .= txid, "output" .= off]


nullOutputLink :: OutputLink
nullOutputLink = OutputLink (TxId "") 0
