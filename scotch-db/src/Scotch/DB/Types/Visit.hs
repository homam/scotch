{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric
#-}

module Scotch.DB.Types.Visit (
    Visit(..)
  , makeVisit
  , CampaignId(..)
  , LandingPage(..)
)
where

import Prelude hiding (concat)
import qualified Database.PostgreSQL.Simple as PS
import Database.PostgreSQL.Simple.ToRow (toRow)
import Database.PostgreSQL.Simple.ToField (toField)
import GHC.Generics (Generic)
import qualified Data.Aeson as A
import qualified Data.Map as M
import qualified Data.Time as Time
import Data.Text.Lazy (Text, fromStrict)
import qualified Network.Wai as Wai
import qualified Data.Text.Encoding as Encoding
import Data.CaseInsensitive (original)
import Scotch.DB.FieldParserHelpers ()
import Scotch.DB.ParsableHelpers ()
import Scotch.DB.Types.GatewayConnection
import Scotch.DB.Types (LandingPage(..), CampaignId(..), OptInMethod(..))
import Scotch.DB.QueryHelpers (defaultTime)
import Control.Arrow ((***))
import Data.Maybe (fromMaybe)
import Control.Monad (msum)

makeVisit :: Maybe GatewayConnection -> CampaignId -> LandingPage -> Wai.Request -> OptInMethod -> Visit
makeVisit gatewayConnection campaignId landingPage req optInMethod =
    let bsToText = fromStrict . Encoding.decodeUtf8
        rawQueryString = Wai.rawQueryString req
        rawPath = bsToText $ Wai.rawPathInfo req
        headers = M.fromList $ map (fromStrict . Encoding.decodeUtf8 . original *** fromStrict . Encoding.decodeUtf8) $ Wai.requestHeaders req
    in Visit {
        visitId = 0 -- auto generated value
      , creationTime = defaultTime -- auto generated value
      , queryParams = M.fromList $ map (fromStrict . Encoding.decodeUtf8 *** fromStrict . nothingToEmpty . fmap Encoding.decodeUtf8) $ Wai.queryString req
      , rawQueryString = bsToText rawQueryString
      , rawPath = rawPath
      , gatewayConnection = gatewayConnection
      , campaignId = campaignId
      , landingPage = landingPage
      , ip = msum $ map (`M.lookup` headers) ["rempte-address"]
      , ipCountry = Nothing
      , headers = headers
      , optInMethod = optInMethod
    }
    where
      nothingToEmpty = fromMaybe ""


data Visit = Visit {
    visitId :: Int
  , creationTime :: Time.ZonedTime
  , campaignId :: CampaignId
  , landingPage :: LandingPage
  , ip :: Maybe Text
  , ipCountry :: Maybe Text
  , headers :: M.Map Text Text
  , queryParams :: M.Map Text Text
  , rawPath :: Text
  , rawQueryString :: Text
  , gatewayConnection :: Maybe GatewayConnection
  , optInMethod :: OptInMethod
} deriving (Show, Generic)

instance PS.ToRow Visit where
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
instance PS.FromRow Visit

-- we want to be able to A.decode visits
instance A.ToJSON Visit
instance A.FromJSON Visit
