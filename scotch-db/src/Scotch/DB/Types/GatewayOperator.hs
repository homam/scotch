{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric
#-}

module Scotch.DB.Types.GatewayOperator (
  GatewayOperator (..)
) where

import Scotch.DB.Types.Imports
import Scotch.DB.FieldParserHelpers ()

-- | Enum of operators
data GatewayOperator = TR_AVEA | TR_TUCKCEL | TR_VODAFONE
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)
instance ToJSON GatewayOperator
instance FromJSON GatewayOperator
