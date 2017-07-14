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
  , addPixel
  , makePixel
  , insertPixel
)
where

import Prelude hiding (concat)
import Data.Text.Lazy (Text, fromStrict, fromStrict, toLower, concat, replace)
import qualified Data.Text as T
import Data.Text (unpack, pack)
import Data.Decimal
import qualified Data.Text.Encoding as Encoding
import qualified Data.Aeson as A
import GHC.Generics (Generic)
import Scotch.DB.FieldParserHelpers ()
import qualified Scotch.DB.Types.Sale as S
import qualified Scotch.DB.Types.Visit as V
import qualified Scotch.DB.Types.PixelValue as PV
import qualified Data.Map as M
import qualified Data.Time as Time
import Scotch.DB.Types.Imports
import System.Random (randomRIO)
-- import Control.Monad.State

addPixel :: (PV.PixelValueId, Double, T.Text) -> S.Sale -> Maybe V.Visit -> Connection -> IO Pixel
addPixel (pixelValueId, pixelFiringRatio, pixelUrl) sale mvisit conn = do
  r <- randomRIO (0, 1::Double)
  let pixel = makePixel (S.salesId sale) (V.visitId <$> mvisit) (fromStrict pixelUrl) (Just pixelValueId) (pixelFiringRatio >= r)
  (_, creationTime) <- insertPixel pixel conn
  return pixel { creationTime }

makePixel :: Int -> Maybe Int -> Text -> Maybe PV.PixelValueId -> Bool -> Pixel
makePixel salesId visitId pixelUrl pixelValueId fireIt = Pixel {
    creationTime = defaultTime
  , lastUpdated = Nothing
  , salesId
  , visitId
  , pixelState = if fireIt then NotProcessed else Scrubbed
  , pixelUrl = Just pixelUrl
  , pixelValueId
  , pixelResultStatusCode = Nothing
  , pixelResultText = Nothing
  }

-- |
-- Usage example:
-- > connectionString <- Env.getEnv "SCOTCH_DB"
-- > pool <- myPool (Char8.pack connectionString)
-- > let query = P.withResource pool
-- > let pixel = Pixel.makePixel 2 (Just 2) "http://www.google.com" M.empty (Just $ Decimal 2 254)
-- > tup <- query (Pixel.insertPixel pixel)
-- > print tup
insertPixel :: Pixel -> Connection -> IO (Int, Time.ZonedTime)
insertPixel v conn = head <$> query
  conn
   [sql|
    insert into pixels (
      last_updated
    , sales_id
    , visit_id
    , pixel_state
    , pixel_url
    , pixel_value_id
    , pixel_result_status_code
    , pixel_result_text
    )
    VALUES (?, ?, ?, ?, ?, ?, ?, ?)
    returning 0, creation_time; |]
  v


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
  , salesId :: Int
  , visitId :: Maybe Int
  , pixelState :: PixelState
  , pixelUrl :: Maybe Text
  , pixelValueId :: Maybe PV.PixelValueId
  , pixelResultStatusCode :: Maybe Int
  , pixelResultText :: Maybe Text
  } deriving (Show, Generic)

instance ToRow Pixel where
  toRow d = [
      -- creation_time -- auto
      toField (lastUpdated d)
    , toField (salesId d)
    , toField (visitId d)
    , toField (pixelState d)
    , toField (pixelUrl d)
    , toField (pixelValueId d)
    , toField (pixelResultStatusCode d)
    , toField (pixelResultText d)
    ]
instance FromRow Pixel

-- we want to be able to A.decode visits
instance A.ToJSON Pixel
instance A.FromJSON Pixel
