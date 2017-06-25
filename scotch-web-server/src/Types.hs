{-# LANGUAGE
  DeriveGeneric
#-}

module Types (
    Postback (..)
) where

import qualified Database.PostgreSQL.Simple as PS
import Database.PostgreSQL.Simple.ToRow (toRow)
import Database.PostgreSQL.Simple.ToField (toField)
import GHC.Generics (Generic)
import qualified Data.Aeson as A


data Postback = Postback {
      integration_payguru_billing_id :: Int
    , transactionid :: String
    , subsid :: String
    , service :: String
    , status :: Int
} deriving (Show, Generic)

instance PS.ToRow Postback where
  toRow d = [
      toField (transactionid d)
    , toField (subsid d)
    , toField (service d)
    , toField (status d)
    ]
instance PS.FromRow Postback

instance A.ToJSON Postback
instance A.FromJSON Postback
