{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric
  , FlexibleInstances
  , UndecidableInstances
  , NamedFieldPuns
  , QuasiQuotes
#-}

{-|
Module      : Scotch.DB.Types.PixelState
Description : Different states of a subscription pixel
-}
module Scotch.DB.Types.Pixel (
    PixelState(..)
  , Pixel(..)
  , makePixel
  , addPixel
)
where

import Prelude hiding (concat)
import Data.Text.Lazy (Text, fromStrict, toLower, concat, replace)
import Data.Text (unpack, pack)
import Data.Decimal
import qualified Data.Text.Encoding as Encoding
import qualified Database.PostgreSQL.Simple as PS
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField (toField)
import Database.PostgreSQL.Simple.ToRow (toRow)
import qualified Data.Aeson as A
import GHC.Generics (Generic)
import Scotch.DB.FieldParserHelpers ()
import qualified Web.Scotty.Trans as Trans
import qualified Network.Wai as Wai
import qualified Data.Map as M
import qualified Data.Time as Time
import Scotch.DB.QueryHelpers (defaultTime)
import Scotch.DB.Types.GatewayConnection
import qualified Web.Scotty as Scotty

makePixel :: Int -> Maybe Int -> Text -> M.Map Text Text -> Maybe Decimal -> Pixel
makePixel subscriberId visitId pixelUrl queryParams pixelValue = Pixel {
    creationTime = defaultTime
  , lastUpdated = Nothing
  , subscriberId
  , visitId
  , pixelState = NotProcessed
  , pixelUrl = Just pixelUrl
  , queryParams = Just queryParams
  , pixelValue
  , pixelResultStatusCode = Nothing
  , pixelResultText = Nothing
  }

data PixelState =
    NotProcessed
  | Cancelled
  | Scrubbed
  | UrlGenerationFailed
  | FiredAndSucceed
  | FiredAndFailed
  | Delayed
  deriving (Show, Read, Eq, Generic, Enum)

instance A.ToJSON PixelState
instance A.FromJSON PixelState

data Pixel = Pixel {
    creationTime :: Time.ZonedTime
  , lastUpdated :: Maybe Time.ZonedTime
  , subscriberId :: Int
  , visitId :: Maybe Int
  , pixelState :: PixelState
  , pixelUrl :: Maybe Text
  , queryParams :: Maybe (M.Map Text Text)
  , pixelValue :: Maybe Decimal
  , pixelResultStatusCode :: Maybe Int
  , pixelResultText :: Maybe Text
  } deriving (Show, Generic)

instance PS.ToRow Pixel where
  toRow d = [
      -- creation_time -- auto
      toField (lastUpdated d)
    , toField (subscriberId d)
    , toField (visitId d)
    , toField (pixelState d)
    , toField (pixelUrl d)
    , toField (queryParams d)
    , toField (pixelValue d)
    , toField (pixelResultStatusCode d)
    , toField (pixelResultText d)
    ]
instance PS.FromRow Pixel

instance A.ToJSON Decimal where
  toJSON = A.toJSON . show
instance A.FromJSON Decimal where
  parseJSON = A.withText "String" (return . read . unpack)

-- we want to be able to A.decode visits
instance A.ToJSON Pixel
instance A.FromJSON Pixel

-- |
-- Usage example:
-- > connectionString <- Env.getEnv "SCOTCH_DB"
-- > pool <- myPool (Char8.pack connectionString)
-- > let query = P.withResource pool
-- > let pixel = Pixel.makePixel 2 (Just 2) "http://www.google.com" M.empty (Just $ Decimal 2 254)
-- > tup <- query (Pixel.addPixel pixel)
-- > print tup
addPixel :: Pixel -> PS.Connection -> IO (Int, Time.ZonedTime)
addPixel v conn = head <$> PS.query
  conn
   [sql|
    insert into pixels (
      last_updated
    , subscriber_id
    , visit_id
    , pixel_state
    , pixel_url
    , query_params
    , pixel_value
    , pixel_result_status_code
    , pixel_resul_text
    )
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
    returning 0, creation_time; |]
  v
