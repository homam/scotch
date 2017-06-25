{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric
  , NamedFieldPuns
  , FlexibleInstances
  , ScopedTypeVariables
  , RankNTypes
  , QuasiQuotes
#-}

module Scotch.DB.Types (
    Visit(..)
  , Postback(..)
)
where

import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Lazy as BL
import qualified Database.PostgreSQL.Simple as PS
import Database.PostgreSQL.Simple.ToRow (toRow)
import Database.PostgreSQL.Simple.ToField (toField, ToField)
import Database.PostgreSQL.Simple.FromField (fromField, FromField)
import GHC.Generics (Generic)
import qualified Data.Aeson as A
import qualified Data.Map as M
import Data.Time.Clock (UTCTime(..))
import qualified Data.Time as Time
import Database.PostgreSQL.Simple.SqlQQ

instance ToField (M.Map String String) where
  toField = toField . A.toJSON

instance FromField (M.Map String String) where
  fromField _ Nothing = return M.empty
  fromField _ (Just bs) = return $ fromMaybe M.empty (A.decode $ BL.fromStrict bs)

data Visit = Visit {
    visit_id :: Int
  , creation_time :: Time.ZonedTime
  , campaign_id :: Int
  , landing_page_id :: Int
  , ip :: String
  , ip_country :: String
  , headers :: Maybe (M.Map String String)
  , query_params :: Maybe (M.Map String String)
} deriving (Show, Generic)

instance PS.ToRow Visit where
  toRow d = [
      -- visit_id -- auto increamenting
      -- creation_time -- auto
      toField (campaign_id d)
    , toField (landing_page_id d)
    , toField (ip d)
    , toField (ip_country d)
    , toField (headers d)
    , toField (query_params d)
    ]
instance PS.FromRow Visit

-- we want to be able to A.decode visits
instance A.ToJSON Visit
instance A.FromJSON Visit


-- | PayGuruPostback
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
