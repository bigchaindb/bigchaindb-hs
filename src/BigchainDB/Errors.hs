{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module BigchainDB.Errors
  ( Err
  , ErrClass(..)
  , allErrorClasses
  , errMsg
  , errStr
  ) where


import Data.Aeson
import qualified Data.Map
import Data.Text

import GHC.Generics


data Err = Err Value
  deriving (Eq, Generic, Show)

instance ToJSON Err where
  toJSON (Err val) = val


data ErrClass =
    InvalidJson
  | InvalidMethod
  | InvalidProtocol
  | InvalidParams
  | TxInvalid
  | TxInvalidFulfillment
  | TxConditionParseError
  | TxSignMissingPrivateKeys
  | TxWrongId
  | HttpError
  | HttpJsonError
  | Http404NotFound
  deriving (Enum, Eq, Generic, Show)

instance ToJSON ErrClass where
  toEncoding = genericToEncoding defaultOptions


allErrorClasses :: [ErrClass]
allErrorClasses = [InvalidJson ..]


errMsg :: ErrClass -> Text -> Err
errMsg code msg = Err $ object [ "class" .= code, "msg" .= msg ]


errStr :: ErrClass -> String -> Err
errStr code msg = Err $ object [ "class" .= code, "msg" .= msg ]
