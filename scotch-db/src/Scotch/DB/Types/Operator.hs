{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric
#-}

module Scotch.DB.Types.Operator (
  Operator (..)
) where

import Scotch.DB.Types.Imports
-- import qualified Data.ByteString.Char8 as Char8
import Scotch.DB.FieldParserHelpers ()

-- | Enum of operators
data Operator = TR_AVEA | TR_TUCKCEL | TR_VODAFONE
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)
instance ToJSON Operator
instance FromJSON Operator

-- instance ToField Operator where
--   toField = toField . show
-- instance FromField Operator where
--   fromField _ Nothing = error "Cannot parse Nothing"
--   fromField _ (Just bs) = return $ read $ Char8.unpack bs
