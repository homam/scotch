{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric
#-}

module Scotch.DB.Types (
    Visit(..)
  , Postback(..)
  , LandingPage(..)
)
where

import qualified Database.PostgreSQL.Simple as PS
import Database.PostgreSQL.Simple.ToRow (toRow)
import Database.PostgreSQL.Simple.ToField (toField)
import GHC.Generics (Generic)
import qualified Data.Aeson as A
import qualified Data.Map as M
import qualified Data.Time as Time
import Scotch.DB.FieldParserHelpers ()
import Data.Text.Lazy (Text)


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

---


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

newtype LandingPage = LandingPage Text
