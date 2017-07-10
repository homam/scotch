{-# LANGUAGE
    OverloadedStrings
#-}

module Scotch.DB.Types.Imports (
    defaultTime
  , Generic
  , PS.Connection, PS.query
  , sql
  , toField, ToField
  , fromField, FromField, returnError, ResultError(..)
  , toRow, PS.FromRow
  , fromRow, PS.ToRow
  , A.fromJSON, A.FromJSON
  , A.toJSON, A.ToJSON
  , Time.ZonedTime
  , Text, fromStrict
  , Decimal
  , showToField
  , readFromField
  , readFromFieldCtor
  , fromFieldJSON
  , toFieldJSON
  , Typeable
)
where

import qualified Database.PostgreSQL.Simple as PS
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToRow (toRow)
import Database.PostgreSQL.Simple.FromRow (fromRow)
import Database.PostgreSQL.Simple.ToField (ToField, toField)
import Database.PostgreSQL.Simple.FromField (FromField, fromField, returnError, ResultError(..))
import GHC.Generics (Generic)
import qualified Data.Aeson as A
import qualified Data.Time as Time
import Data.Text.Lazy (Text, fromStrict)
import Data.Decimal
import Data.Typeable.Internal (Typeable)
-- import qualified Data.Time as Time
-- import Data.Text.Lazy (Text, fromStrict)
-- import qualified Network.Wai as Wai
-- import qualified Data.Text.Encoding as Encoding
import Scotch.DB.FieldParserHelpers (showToField
  , readFromField
  , readFromFieldCtor
  , fromFieldJSON
  , toFieldJSON)
import Scotch.DB.ParsableHelpers ()
import Scotch.DB.QueryHelpers (defaultTime)
