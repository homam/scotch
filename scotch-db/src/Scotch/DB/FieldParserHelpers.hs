{-# LANGUAGE
    OverloadedStrings
  , FlexibleInstances
#-}

module Scotch.DB.FieldParserHelpers (
)
where

import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Lazy as BL
import Data.Text.Lazy (Text)
-- import qualified Database.PostgreSQL.Simple (ToRow)
import Database.PostgreSQL.Simple.ToField (toField, ToField)
import Database.PostgreSQL.Simple.FromField (fromField, FromField)
import Database.PostgreSQL.Simple.ToRow (toRow, ToRow)
import qualified Data.Aeson as A
import qualified Data.Map as M

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
