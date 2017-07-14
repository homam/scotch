{-# LANGUAGE
    OverloadedStrings
  , QuasiQuotes
  , DeriveGeneric
  , TupleSections
  , GeneralizedNewtypeDeriving
  , FlexibleContexts
  , NamedFieldPuns
#-}

module Scotch.DB.Types.PixelValue (
    pixelValue
  , addNewPixelValueDB
  , PixelValueId (..)
  , PixelValueUrlRepresentationConfig (..)
  , pixelUrl
  , HandsetLevel (..)
  , dbToPixelValue -- maybe ?
) where

import Prelude hiding (concat)
import qualified Data.Map as M
import Scotch.DB.Types.Imports
import Scotch.DB.Types.GatewayOperator (GatewayOperator)
import Scotch.DB.Types.GatewayConnection
import Scotch.DB.Types.Affiliate (Affiliate, getPixelUrl, AffiliateId (..), PixelValueUrlRepresentation (..), HandsetLevel (..))
import qualified Scotch.DB.Types.Affiliate as AF
import Scotch.DB.Types (CampaignId)
-- import Scotch.DB.Types.Affiliate (AffiliateId, HandsetLevel, PixelValueUrlRepresentation)

import Scotch.DB.FieldParserHelpers ()
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (pack, concat, toStrict)
import qualified Data.Text as T
import Control.Monad.Reader (MonadReader, ask, Reader, runReader)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import Control.Monad.Except (MonadError)

-- | Read-only configuration that is used in `pixelValue` function.
data PixelValueUrlRepresentationConfig = PixelValueUrlRepresentationConfig {
    campaignIdMap :: M.Map CampaignId Affiliate
  , pixelMap :: M.Map (GatewayConnection, Maybe GatewayOperator, Maybe AffiliateId, Maybe HandsetLevel) (Maybe PixelValue)
  } deriving (Show)

newtype PixelValueUrlRepresentationGetter a = PixelValueUrlRepresentationGetter {
    unPixelValueUrlRepresentationGetter :: ExceptT Text (Reader PixelValueUrlRepresentationConfig) a
  } deriving (Functor, Applicative, Monad, MonadReader PixelValueUrlRepresentationConfig, MonadError Text)

runPixelValueUrlRepresentationGetter = runReader . runExceptT . unPixelValueUrlRepresentationGetter

pixelUrl :: GatewayConnection -> GatewayOperator -> CampaignId -> HandsetLevel -> PixelValueUrlRepresentationConfig -> QueryString -> Either T.Text (Maybe (PixelValueId, Double, T.Text))
pixelUrl g o cid h = pixelUrl' $ pixelValue g o cid h

pixelUrl' :: PixelValueUrlRepresentationGetter (Maybe (Affiliate, PixelValue)) -> PixelValueUrlRepresentationConfig -> QueryString -> Either T.Text (Maybe (PixelValueId, Double, T.Text))
pixelUrl' getter config visitQueryString =
  case runPixelValueUrlRepresentationGetter getter config of
    Left e -> Left (toStrict e)
    Right mpv -> Right $ fmap go mpv
  where
    go (affiliate, (pixelValueId, probability, pixelValue)) = (pixelValueId, probability, getPixelUrl affiliate visitQueryString pixelValue)

pixelValue :: GatewayConnection -> GatewayOperator -> CampaignId -> HandsetLevel -> PixelValueUrlRepresentationGetter (Maybe (Affiliate, PixelValue))
pixelValue g o cid h = do
  c <- ask
  case M.lookup cid (campaignIdMap c) of
    Nothing -> PixelValueUrlRepresentationGetter $ throwE $ concat ["No affiliate was found for CampaignId = ", pack $ show cid]
    Just aff -> return $ (aff, ) <$> runReader (go g o (AF.affiliateId aff) h) ( pixelMap c)
    where
      -- hardcoded rules
      -- go PayguruTurkey TR_AVEA (AffiliateId "BillyMob") _ = return $ Just (PixelValueId 101, 1, DecimalPixelValueUrlRepresentation $ Decimal 2 110
      -- generic rule
      go g o a h = do
        m <- ask
        return $ find m [
            (g, Just o, Just a, Just h)
          , (g, Just o, Just a, Nothing)
          , (g, Just o, Nothing, Just h)
          , (g, Nothing, Nothing, Nothing)
          ]

      find _ [] =  Just (PixelValueId 0, 1, NoPixelValueUrlRepresentation) -- by default fire all the pixels
      find m (x:xs) = fromMaybe (find m xs) (M.lookup x m)

newtype PixelValueId = PixelValueId Int
  deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON PixelValueId
instance FromJSON PixelValueId

instance ToField PixelValueId where
  toField (PixelValueId i) = toField i

instance FromField PixelValueId where
  fromField = readFromFieldCtor "PixelValueId" PixelValueId

type PixelValue = (PixelValueId, Double, PixelValueUrlRepresentation)

dbToPixelValue :: PixelValueDB -> PixelValue
dbToPixelValue p = (pixelValueId p, pixelFiringRatio p, pixelValueUrlRep p)

{--
data PSQLLogicalAny v = Any | Strictly v deriving (Show, Read, Generic)

instance ToField v => ToField (PSQLLogicalAny v) where
  toField Any = toField ("-" :: Text)
  toField (Strictly v) = toField v

instance (FromField v, Typeable v) => FromField (PSQLLogicalAny v) where
  fromField f Nothing = returnError UnexpectedNull f ""
  fromField f (Just bs) = if bs == "-" then return Any else Strictly <$> fromField f (Just bs)

fromMaybe' :: Maybe a -> PSQLLogicalAny a
fromMaybe' = maybe Any Strictly

toMaybe' :: PSQLLogicalAny a -> Maybe a
toMaybe' Any = Nothing
toMaybe' (Strictly a) = Just a
--}




data PixelValueDB = PixelValueDB {
    pixelValueId :: PixelValueId -- primary key serial
  , creationTime :: ZonedTime -- automatic
  , createdBy :: Text -- mandatory
  , gatewayConnection :: GatewayConnection -- mandatory
  , operator :: Maybe GatewayOperator -- null means any
  , affiliateId :: Maybe AffiliateId -- null means any
  , handsetLevel :: Maybe HandsetLevel -- null means any
  , hardcodedValueDescription :: Maybe Text -- null means not hardcoded
  , pixelFiringRatio :: Double -- 0 means all pixels get scrubbed, 1 means all will be fired
  , pixelValueUrlRep :: PixelValueUrlRepresentation -- mandqtory
  , pixelMonetaryValue :: Maybe Decimal -- null means undefined
  } deriving (Show, Read, Generic)


addNewPixelValueDB ::
 Text ->
 GatewayConnection ->
 Maybe GatewayOperator ->
 Maybe AffiliateId ->
 Maybe HandsetLevel ->
 Maybe Text ->
 Double ->
 PixelValueUrlRepresentation ->
 Maybe Decimal ->
 Connection -> IO (Int, ZonedTime)
addNewPixelValueDB
  createdBy
  gatewayConnection
  operator
  affiliateId
  handsetLevel
  hardcodedValueDescription
  pixelFiringRatio
  pixelValueUrlRep
  pixelMonetaryValue
  = insertNewPixelValueDB PixelValueDB {
    pixelValueId = PixelValueId 0
  , creationTime = defaultTime
  , createdBy
  , gatewayConnection
  , operator
  , affiliateId
  , handsetLevel
  , hardcodedValueDescription
  , pixelFiringRatio
  , pixelValueUrlRep
  , pixelMonetaryValue
  }

insertNewPixelValueDB :: PixelValueDB -> Connection -> IO (Int, ZonedTime)
insertNewPixelValueDB v conn = head <$> query
  conn
   [sql|
    insert into pixel_values (
      created_by
    , gateway_connection
    , operator
    , affiliate_id
    , handset_level
    , hardcoded_value_description
    , pixel_firing_ratio
    , pixel_value_url_rep
    , pixel_monetary_value
    )
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
    returning 0, creation_time; |]
  v

instance ToRow PixelValueDB where
  toRow d = [
      toField (createdBy d)
    , toField (gatewayConnection d)
    , toField (operator d)
    , toField (affiliateId d)
    , toField (handsetLevel d)
    , toField (hardcodedValueDescription d)
    , toField (pixelFiringRatio d)
    , toField (pixelValueUrlRep d)
    , toField (pixelMonetaryValue d)
    ]

instance FromRow PixelValueDB
