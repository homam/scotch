{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric
#-}

module Scotch.DB.Types (
    module Scotch.DB.Types
  , LandingPage(..)
  , CampaignId(..)
  , OptInMethod(..)
  , ServiceId(..)
)
where

import Prelude hiding (concat)
import Scotch.DB.FieldParserHelpers ()
import Scotch.DB.ParsableHelpers ()
import Data.Text.Lazy (Text, toStrict, fromStrict, unpack)
import Database.PostgreSQL.Simple.ToField (toField, ToField)
import Database.PostgreSQL.Simple.FromField (FromField(..), returnError, ResultError(..))
import qualified Data.Text.Encoding as Encoding
import GHC.Generics (Generic)
import qualified Data.Aeson as A
import qualified Data.Map as M
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T


newtype LandingPage = LandingPage Text deriving (Show, Read, Generic)

instance A.ToJSON LandingPage
instance A.FromJSON LandingPage

instance ToField LandingPage where
  toField (LandingPage lp) = toField lp
instance FromField LandingPage where
  fromField f Nothing = returnError UnexpectedNull f ""
  fromField _ (Just bs) = return $ LandingPage (fromStrict $ Encoding.decodeUtf8 bs)

---

fromFieldN _ _ f Nothing = returnError UnexpectedNull f ""
fromFieldN ctor cast f (Just bs) = case cast $ unpack $ fromStrict $ Encoding.decodeUtf8 bs of
  (c, _):_ -> return $ ctor c
  [] -> returnError ConversionFailed f ""


-- | CampaignId
newtype CampaignId = CampaignId Int deriving (Show, Read, Generic, Eq, Ord)

instance A.ToJSON CampaignId
instance A.FromJSON CampaignId

instance ToField CampaignId where
  toField (CampaignId lp) = toField lp
instance FromField CampaignId where
  fromField f Nothing = returnError UnexpectedNull f ""
  fromField f (Just bs) = case reads $ unpack $ fromStrict $ Encoding.decodeUtf8 bs of
    (c, _):_ -> return $ CampaignId c
    [] -> returnError ConversionFailed f ""

-- | ServiceId

newtype ServiceId = ServiceId String deriving (Show, Read, Generic)

instance A.ToJSON ServiceId
instance A.FromJSON ServiceId

instance ToField ServiceId where
  toField (ServiceId s) = toField s
instance FromField ServiceId where
  fromField f Nothing = returnError UnexpectedNull f ""
  fromField _ (Just bs) = return $ ServiceId $ unpack $ fromStrict $ Encoding.decodeUtf8 bs


data OptInMethod = RedirectToPaymentPage
  deriving (Show, Read, Eq, Generic, Enum, Bounded)

instance A.ToJSON OptInMethod
instance A.FromJSON OptInMethod


type QueryString = M.Map (CI.CI T.Text) T.Text

queryStringFromMap :: M.Map T.Text T.Text -> QueryString
queryStringFromMap = M.mapKeys (CI.mk . clean)
  where
    clean k = if T.isPrefixOf "?" k then T.tail k else k

queryStringFromLazyMap :: M.Map Text Text -> QueryString
queryStringFromLazyMap = queryStringFromMap . M.mapKeys toStrict . M.map toStrict

queryStringToMap :: QueryString -> M.Map T.Text T.Text
queryStringToMap = M.mapKeys (T.toLower . CI.original)
