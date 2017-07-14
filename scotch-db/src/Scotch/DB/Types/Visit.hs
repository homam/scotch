{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric
  , QuasiQuotes
  , NamedFieldPuns
#-}

module Scotch.DB.Types.Visit (
    Visit(..)
  , makeVisit
  , insertVisit
  , getAllVisits
  , getOneVisit
  , CampaignId(..)
  , LandingPage(..)
)
where

import Prelude hiding (concat)
import Scotch.DB.Types.Imports
import qualified Data.Map as M
import Data.Text.Lazy (Text, fromStrict)
import qualified Network.Wai as Wai
import qualified Data.Text.Encoding as Encoding
import Data.CaseInsensitive (original)
import Scotch.DB.FieldParserHelpers ()
import Scotch.DB.ParsableHelpers ()
import Scotch.DB.Types.GatewayConnection
import Scotch.DB.Types (LandingPage(..), CampaignId(..), OptInMethod(..))
import Control.Arrow ((***))
import Data.Maybe (fromMaybe)
import Control.Monad (msum)

-- | Creates a 'Visit' instance using 'Wai.Request'
makeVisit :: Maybe GatewayConnection -> CampaignId -> LandingPage -> Wai.Request -> OptInMethod -> Visit
makeVisit gatewayConnection campaignId landingPage req optInMethod =
    let bsToText = fromStrict . Encoding.decodeUtf8
        rawQueryString = bsToText $ Wai.rawQueryString req
        rawPath = bsToText $ Wai.rawPathInfo req
        headers = M.fromList $ map (fromStrict . Encoding.decodeUtf8 . original *** fromStrict . Encoding.decodeUtf8) $ Wai.requestHeaders req
        queryParams = queryStringFromLazyMap $ M.fromList $ map (fromStrict . Encoding.decodeUtf8 *** fromStrict . nothingToEmpty . fmap Encoding.decodeUtf8) $ Wai.queryString req
        ipCountry = Nothing
    in Visit {
        visitId = 0 -- auto generated value
      , creationTime = defaultTime -- auto generated value
      , queryParams
      , rawQueryString
      , rawPath
      , gatewayConnection
      , campaignId
      , landingPage
      , ip = msum $ map (`M.lookup` headers) ["remote-address"]
      , ipCountry
      , headers
      , optInMethod
    }
    where
      nothingToEmpty = fromMaybe ""

-- | Inserts the given Visit instance into 'visits' table
insertVisit :: Visit -> Connection -> IO (Int, ZonedTime)
insertVisit v conn = head <$> query
  conn
   [sql|
    insert into visits (
      campaign_id
    , landing_page
    , ip
    , ip_country
    , headers
    , query_params
    , raw_path
    , raw_query_string
    , gateway_connection
    , opt_in_method
    )
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    returning visit_id, creation_time; |]
  v

getAllVisits :: Connection -> IO [Visit]
getAllVisits conn = query_ conn [sql|
  select
    visit_id
  , creation_time
  , campaign_id
  , landing_page
  , text(ip) as ip
  , ip_country
  , headers
  , query_params
  , raw_path
  , raw_query_string
  , gateway_connection
  , opt_in_method
  from visits order by visit_id desc limit 10; |]


getOneVisit :: Int -> Connection -> IO (Maybe Visit)
getOneVisit visitId conn = safeHead <$> query conn [sql|
  select
    visit_id
  , creation_time
  , campaign_id
  , landing_page
  , text(ip) as ip
  , ip_country
  , headers
  , query_params
  , raw_path
  , raw_query_string
  , gateway_connection
  , opt_in_method
  from visits
  where visit_id = ?
  limit ?; |]
  (visitId, 1 :: Int)

data Visit = Visit {
    visitId :: Int
  , creationTime :: ZonedTime
  , campaignId :: CampaignId
  , landingPage :: LandingPage
  , ip :: Maybe Text
  , ipCountry :: Maybe Text
  , headers :: M.Map Text Text
  , queryParams :: QueryString
  , rawPath :: Text
  , rawQueryString :: Text
  , gatewayConnection :: Maybe GatewayConnection
  , optInMethod :: OptInMethod
} deriving (Show, Generic)

instance ToRow Visit where
  toRow d = [
      -- visit_id -- auto increamenting
      -- creation_time -- auto
      toField (campaignId d)
    , toField (landingPage d)
    , toField (ip d)
    , toField (ipCountry d)
    , toField (headers d)
    , toField (queryParams d)
    , toField (rawPath d)
    , toField (rawQueryString d)
    , toField (gatewayConnection d)
    , toField (optInMethod d)
    ]
instance FromRow Visit

-- we want to be able to A.decode visits
instance ToJSON Visit
instance FromJSON Visit
