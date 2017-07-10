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
import Data.Text.Lazy (Text)
import Database.PostgreSQL.Simple.ToField (toField, ToField, Action)
import Database.PostgreSQL.Simple.FromField (fromField, FromField, returnError, ResultError(..))
import Database.PostgreSQL.Simple.Internal (Field, Conversion)
import qualified Data.Aeson as A
import qualified Data.Map as M
import Data.Decimal (Decimal)
import Data.Text (unpack)

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

instance ToField (Text, Text) where
  toField = toField . A.toJSON

instance FromField (Text, Text) where
  fromField _ Nothing = return ("","")
  fromField _ (Just bs) = return $ fromMaybe ("", "") (A.decode $ BL.fromStrict bs)

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
