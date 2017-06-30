{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric
#-}

module Scotch.DB.Types.GatewayConnection (
  GatewayConnection(..)
)
where

import Prelude hiding (concat)
import GHC.Generics (Generic)
import qualified Data.Aeson as A
import qualified Data.Map as M
import Scotch.DB.FieldParserHelpers ()
import Scotch.DB.ParsableHelpers ()
import Data.Text.Lazy (toLower, concat, pack)
import qualified Web.Scotty as Scotty
import Database.PostgreSQL.Simple.ToField (ToField(..))
import Database.PostgreSQL.Simple.FromField (FromField(..), returnError, ResultError(..))
import qualified Data.ByteString.Lazy as BL

data GatewayConnection = PayguruTurkey | TestStandard
  deriving (Show, Read, Eq, Generic, Enum, Bounded)

instance A.ToJSON GatewayConnection
instance A.FromJSON GatewayConnection

instance Scotty.Parsable GatewayConnection where
  parseParam txt =
    let dic = M.fromList $ map (\g -> (toLower . pack . show $ g, g)) [minBound ..]
    in case M.lookup (toLower txt) dic of
      Just gw -> Right gw
      Nothing -> Left $ concat ["Unable to parse ", txt, " to a GatewayConnection"]
