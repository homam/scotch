{-# LANGUAGE
    OverloadedStrings
  , FlexibleInstances
  , UndecidableInstances
  , RankNTypes
#-}

module Scotch.DB.FieldParserHelpers (
    showToField
  , readFromField
  , readFromFieldCtor
  , fromFieldJSON
  , toFieldJSON
)
where

import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as Char8
import Data.Typeable.Internal (Typeable)
import Database.PostgreSQL.Simple.ToField (toField, ToField, Action)
import Database.PostgreSQL.Simple.FromField (fromField, FromField, returnError, ResultError(..))
import Database.PostgreSQL.Simple.Internal (Field, Conversion)
import qualified Data.Aeson as A
import Data.Aeson.Types
import qualified Data.Map as M
import Data.Decimal (Decimal)
import Data.Text (unpack)
import qualified Data.Text as T
import qualified Data.CaseInsensitive as CI
-- import qualified Data.Text.Encoding as Encoding

instance ToField (M.Map String String) where
  toField = toField . A.toJSON

instance FromField (M.Map String String) where
  fromField _ Nothing = return M.empty
  fromField _ (Just bs) = return $ fromMaybe M.empty (A.decode $ BL.fromStrict bs)

instance {-# OVERLAPPABLE #-} (A.FromJSONKey a, Ord a, A.FromJSON b) => FromField (M.Map a b) where
  fromField _ Nothing = return M.empty
  fromField _ (Just bs) = return $ fromMaybe M.empty (A.decode $ BL.fromStrict bs)

instance {-# OVERLAPPABLE #-} (A.ToJSONKey a, A.ToJSON b) => ToField (M.Map a b) where
  toField = toField . A.toJSON

instance ToField (T.Text, T.Text) where
  toField = toField . A.toJSON

instance FromField (T.Text, T.Text) where
  fromField _ Nothing = return ("","")
  fromField _ (Just bs) = return $ fromMaybe ("", "") (A.decode $ BL.fromStrict bs)

instance ToField (CI.CI T.Text) where
  toField = toField . T.toLower . CI.original

instance A.ToJSON (CI.CI T.Text) where
  toJSON = toJSON . T.toLower . CI.original

instance A.FromJSON (CI.CI T.Text) where
  parseJSON = withText "Text" (\t -> CI.mk <$> pure t)

instance A.ToJSONKey (CI.CI T.Text) where
  toJSONKey = toJSONKeyText (T.toLower . CI.original)

instance A.FromJSONKey (CI.CI T.Text) where
  fromJSONKey = FromJSONKeyText CI.mk

-- instance FromField (CI.CI Text) where
--   fromField f Nothing = returnError UnexpectedNull f ""
--   fromField f (Just bs) = return (CI.mk $ Encoding.decodeUtf8 $ BL.fromStrict bs)

instance {-# OVERLAPPABLE #-} A.FromJSON a => FromField [a] where
  fromField _ Nothing = return []
  fromField _ (Just bs) = return $ fromMaybe [] (A.decode $ BL.fromStrict bs)

instance {-# OVERLAPPABLE #-} A.ToJSON a => ToField [a] where
  toField = toField . A.toJSON

showToField :: forall a. Show a => a -> Action
showToField = toField . show

readFromField :: forall a. Read a => Field -> Maybe Char8.ByteString -> Conversion a
readFromField _ Nothing = error "Cannot parse Nothing"
readFromField _ (Just bs) = return $ read $ Char8.unpack bs

readFromFieldCtor name _ f Nothing = returnError UnexpectedNull f ("Cannot parse null DB value to " ++ name)
readFromFieldCtor name ctor f (Just bs) = case reads (Char8.unpack bs) of
  [] -> returnError Incompatible f ("Unable to parse Db value to " ++ name)
  (v, _):_ -> return $ ctor v

instance {-# OVERLAPPABLE #-} (Show a) => ToField a where
    toField = showToField
instance {-# OVERLAPPABLE #-} (Read a) => FromField a where
    fromField = readFromField

toFieldJSON :: A.ToJSON a => a -> Action
toFieldJSON = toField . A.toJSON

fromFieldJSON :: (A.FromJSON a, Typeable a) => Field -> Maybe Char8.ByteString -> Conversion a
fromFieldJSON f Nothing = returnError UnexpectedNull f "Unable to parse null to JSON"
fromFieldJSON f (Just bs) = case A.decode $ BL.fromStrict bs of
  Nothing -> returnError Incompatible f "Unable to parse JSON"
  Just v  -> return v

instance A.ToJSON Decimal where
  toJSON = A.toJSON . show
instance A.FromJSON Decimal where
  parseJSON = A.withText "String" (return . read . unpack)
