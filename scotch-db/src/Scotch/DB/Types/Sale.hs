{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric
  , QuasiQuotes
  , NamedFieldPuns
#-}

module Scotch.DB.Types.Sale (
    Sale(..)
  , insertSale
)
where

import Prelude hiding (concat)
import Scotch.DB.Types.Imports
import Scotch.DB.FieldParserHelpers ()
import Scotch.DB.ParsableHelpers ()
import Scotch.DB.Types.GatewayConnection
import Scotch.DB.Types.GatewayOperator
import Scotch.DB.Types.GatewayService

-- | Inserts the given Sale instance into 'visits' table
insertSale :: Sale -> Connection -> IO (Int, ZonedTime)
insertSale v conn = head <$> query
  conn
   [sql|
    insert into sales (
      gateway
    , operator
    , service
    , is_active
    , unsubscription_time
    , visit_id
    )
    VALUES (?, ?, ?, ?, ?, ?)
    returning sales_id, creation_time; |]
  v

data Sale = Sale {
    salesId :: Int
  , creationTime :: ZonedTime
  , gatewayConnection :: GatewayConnection
  , gatewayOperator :: GatewayOperator
  , service :: GatewayService
  , isActive :: Bool
  , unsubscriptionTime :: Maybe ZonedTime
  , visitId :: Maybe Int
} deriving (Show, Generic)

instance ToRow Sale where
  toRow d = [
      -- salesID -- auto increamenting
      -- creationTime -- auto
      toField (gatewayConnection d)
    , toField (gatewayOperator d)
    , toField (service d)
    , toField (isActive d)
    , toField (unsubscriptionTime d)
    , toField (visitId d)
    ]
instance FromRow Sale

-- we want to be able to A.decode visits
instance ToJSON Sale
instance FromJSON Sale
