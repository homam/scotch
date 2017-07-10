{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric
#-}

module Scotch.DB.Types.HandsetLevel (
  HandsetLevel (..)
) where

import Scotch.DB.Types.Imports
import Scotch.DB.FieldParserHelpers ()

-- | Various types of handset levels that affect the value of pixels
data HandsetLevel = HighEnd | MidLevel | LowEnd
  deriving (Show, Read, Eq, Ord, Generic, Enum, Bounded)
instance ToJSON HandsetLevel
instance FromJSON HandsetLevel
