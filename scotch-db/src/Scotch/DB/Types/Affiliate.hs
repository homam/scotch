{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric
  , NamedFieldPuns
  , QuasiQuotes
#-}

module Scotch.DB.Types.Affiliate (
    HandsetLevel (..)
  , Affiliate (..)
  , AffiliateId (..)
  , addNewAffiliateDB
  , standardAffiliate
  , PixelValueUrlRepresentation (..)
  , pixelValueToMap
) where

import Scotch.DB.Types.Imports
import Scotch.DB.Types.GatewayOperator (GatewayOperator)
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Decimal (Decimal)
import Scotch.DB.FieldParserHelpers ()
import Scotch.DB.Types.HandsetLevel (HandsetLevel(..))

-- | Affiliate Id
newtype AffiliateId = AffiliateId String
  deriving (Show, Read, Eq, Ord, Generic)

-- | Affiliates are instances of this type
data Affiliate = Affiliate {
    getPixelUrl :: QueryString -> PixelValueUrlRepresentation -> T.Text
  , affiliateId :: AffiliateId
  }

instance Show Affiliate where
  show = show . affiliateId

-- | Constructs a standard affiliate
-- Visit query string of the standard affiliate: ?clickid=123456&publisher=abcdef
-- URL template: http://www.affiliate.com/tack?click_id={clickid}&pixel={pixel_value}
-- > getPixelUrl standard (M.fromList [("clickid", "123456"), ("publisher", "abcdef")]) (DecimalPixelValueUrlRepresentation 1.8)
-- > -- Constructed Pixel URL: http://www.affiliate.com/tack?click_id=123456&pixel=1.8
standardAffiliate :: AffiliateId -> T.Text -> Affiliate
standardAffiliate affId turl = Affiliate {
    getPixelUrl = \ visitQueryString pixelValue -> formatText (M.union visitQueryString (pixelValueToMap pixelValue)) turl
  , affiliateId = affId
  }

-- | Represents different ways of representing the value of the pixel in the URL
data PixelValueUrlRepresentation =
    DecimalPixelValueUrlRepresentation Decimal -- ^ Value of pixel is represneted by a decimal number in the URL
  | PerOperatorPixelValueUrlRepresentation GatewayOperator -- ^ Value is represented by the name of the operator on the URL
  | ParametricPixelValueUrlRepresentation (M.Map T.Text T.Text) -- ^ Customizable
  | NoPixelValueUrlRepresentation -- ^ Pixel value is not represented in the URL
  deriving (Show, Read, Eq, Generic)

instance ToJSON PixelValueUrlRepresentation
instance FromJSON PixelValueUrlRepresentation

instance ToField PixelValueUrlRepresentation where
  toField = toFieldJSON
instance FromField PixelValueUrlRepresentation where
  fromField = fromFieldJSON

-- | Converts a 'PixelValueUrlRepresentation' to a Map that can be used for constructing a URL
pixelValueToMap :: PixelValueUrlRepresentation -> QueryString
pixelValueToMap = queryStringFromMap . pixelValueToMap' where
  pixelValueToMap' (DecimalPixelValueUrlRepresentation d) = M.fromList [("pixle_value", T.pack $ show d)]
  pixelValueToMap' (PerOperatorPixelValueUrlRepresentation o) = M.fromList [("operator", T.pack $ show o)]
  pixelValueToMap' (ParametricPixelValueUrlRepresentation m) = m
  pixelValueToMap' NoPixelValueUrlRepresentation = M.empty

-- | Utility function for formatting texts by mustache syntax: "{parameter}"
formatText :: QueryString -> T.Text -> T.Text
formatText m t = foldl
  (\x (k, v) -> T.replace (T.concat ["{", k, "}"]) v x)
  t
  (M.toList $ queryStringToMap m)

data AffiliateDB = AffiliateDB {
    dbAffiliateId :: T.Text
  , dbCreationTime :: ZonedTime
  , dbCreatedBy :: T.Text
  , dbPixelUrlTemplate :: T.Text
} deriving (Show, Read, Generic)


instance ToRow AffiliateDB where
  toRow d = [
      toField (dbAffiliateId d)
    -- , toField (dbCreationTime d) -- automatic
    , toField (dbCreatedBy d)
    , toField (dbPixelUrlTemplate d)
    ]

instance FromRow AffiliateDB

insertNewAffiliateDB :: AffiliateDB -> Connection -> IO (Int, ZonedTime)
insertNewAffiliateDB v conn = head <$> query
  conn
   [sql|
    insert into affiliates (
      affiliate_id
    , created_by
    , pixel_url_template
    )
    VALUES (?, ?, ?)
    returning 0, creation_time; |]
  v

addNewAffiliateDB :: T.Text -> T.Text -> T.Text -> Connection -> IO (Int, ZonedTime)
addNewAffiliateDB dbAffiliateId dbCreatedBy dbPixelUrlTemplate = insertNewAffiliateDB AffiliateDB {
    dbAffiliateId
  , dbCreationTime = defaultTime
  , dbCreatedBy
  , dbPixelUrlTemplate
  }
