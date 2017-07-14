{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric
#-}

module Scotch.DB.Types.GatewayService (
  GatewayService (..)
) where

import Scotch.DB.Types.Imports
import Scotch.DB.FieldParserHelpers ()

-- | Enum of operators
data GatewayService = AppRing
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)
instance ToJSON GatewayService
instance FromJSON GatewayService
