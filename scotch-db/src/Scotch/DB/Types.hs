{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric
#-}

module Scotch.DB.Types (
    module Scotch.DB.Types
  , LandingPage(..)
  , CampaignId(..)
  , OptInMethod(..)
)
where

import Prelude hiding (concat)
import Scotch.DB.FieldParserHelpers ()
import Scotch.DB.ParsableHelpers ()
import Data.Text.Lazy (Text, fromStrict, unpack)
import Database.PostgreSQL.Simple.ToField (toField, ToField)
import Database.PostgreSQL.Simple.FromField (FromField(..), returnError, ResultError(..))
import qualified Data.Text.Encoding as Encoding
import GHC.Generics (Generic)
import qualified Data.Aeson as A


newtype LandingPage = LandingPage Text deriving (Show, Read, Generic)

instance A.ToJSON LandingPage
instance A.FromJSON LandingPage

instance ToField LandingPage where
  toField (LandingPage lp) = toField lp
instance FromField LandingPage where
  fromField f Nothing = returnError UnexpectedNull f ""
  fromField _ (Just bs) = return $ LandingPage (fromStrict $ Encoding.decodeUtf8 bs)


newtype CampaignId = CampaignId Int deriving (Show, Read, Generic)

instance A.ToJSON CampaignId
instance A.FromJSON CampaignId

instance ToField CampaignId where
  toField (CampaignId lp) = toField lp
instance FromField CampaignId where
  fromField f Nothing = returnError UnexpectedNull f ""
  fromField f (Just bs) = case reads $ unpack $ fromStrict $ Encoding.decodeUtf8 bs of
    (c, _):_ -> return $ CampaignId c
    [] -> returnError ConversionFailed f ""


data OptInMethod = RedirectToPaymentPage
  deriving (Show, Read, Eq, Generic, Enum, Bounded)

instance A.ToJSON OptInMethod
instance A.FromJSON OptInMethod
