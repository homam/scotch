{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric
  , FlexibleInstances
  , UndecidableInstances
  , QuasiQuotes
#-}

{-|
Module      : Scotch.DB.Types.GatewayNotification
Description : Represents a PostgreSQL record of a generic HTTP Postback Notification from a Gateway
-}
module Scotch.DB.Types.GatewayNotification (
    GatewayNotification(..)
  , makeGatewayNotification, makeGatewayNotification'
  , NotificationType(..)
  , GatewayConnection(..)
  , AsyncTaskStatus(..)
  , insertGatewayNotification
  , getOneNotification, getAllNotifications
)
where

import Prelude hiding (concat)
import Data.Text.Lazy (Text, fromStrict, toLower, concat, pack, replace)
import qualified Data.Text.Encoding as Encoding
import qualified Database.PostgreSQL.Simple as PS
import Database.PostgreSQL.Simple.ToField (toField)
import Database.PostgreSQL.Simple.ToRow (toRow)
import qualified Data.Aeson as A
import GHC.Generics (Generic)
import Scotch.DB.FieldParserHelpers ()
import qualified Web.Scotty.Trans as Trans
import qualified Network.Wai as Wai
import qualified Data.Map as M
import qualified Data.Time as Time
import Scotch.DB.Types.Imports
import Scotch.DB.QueryHelpers (defaultTime, safeHead)
import Scotch.DB.Types.GatewayConnection
import qualified Web.Scotty as Scotty


makeGatewayNotification :: Wai.Request -> GatewayConnection -> NotificationType -> [Trans.Param] -> GatewayNotification
makeGatewayNotification req gatewayConnection notificationType allParams =
    let bsToText = fromStrict . Encoding.decodeUtf8
        rawPath = bsToText $ Wai.rawPathInfo req
        rawQueryString = bsToText $ Wai.rawQueryString req
    in makeGatewayNotification' gatewayConnection notificationType allParams rawPath rawQueryString

makeGatewayNotification' :: GatewayConnection -> NotificationType -> [Trans.Param] -> Text -> Text -> GatewayNotification
makeGatewayNotification' gatewayConnection notificationType allParams rawPath rawQueryString =
  GatewayNotification {
      gatewayNotificationId = 0 -- auto generated value
    , creationTime = defaultTime -- auto generated value
    , allParams = M.fromList allParams
    , rawQueryString = rawQueryString
    , rawPath = rawPath
    , notificationType = notificationType
    , gatewayConnection = gatewayConnection
    , taskStatus = NotStarted -- default value
    , taskResult = Nothing -- defauklt value
    , taskLastUpdatedTime = Nothing
  }

insertGatewayNotification :: GatewayNotification -> PS.Connection -> IO (Int, Time.ZonedTime)
insertGatewayNotification notification conn = head <$> PS.query
  conn
  [sql|
    insert into gateway_notifications (
      all_params
    , raw_path
    , raw_query_string
    , notification_type
    , gateway_connection
    , task_status
    , task_result
    , task_last_updated_time
    )
    VALUES (?, ?, ?, ?, ?, ?, ?, ?) returning gateway_notification_id, creation_time; |]
  notification

getAllNotifications :: AsyncTaskStatus -> NotificationType -> Int -> PS.Connection -> IO [GatewayNotification]
getAllNotifications tstatus ntype limit conn = PS.query
  conn
  [sql|
    select
      gateway_notification_id
    , creation_time
    , all_params
    , raw_path
    , raw_query_string
    , notification_type
    , gateway_connection
    , task_status
    , task_result
    , task_last_updated_time
    from gateway_notifications
    where task_status = ?
      and notification_type = ?
    order by gateway_notification_id desc
    limit ?
    |]
  (tstatus, ntype, limit)

getOneNotification :: Int -> PS.Connection -> IO (Maybe GatewayNotification)
getOneNotification gatewayNotificationId conn = safeHead <$> PS.query
  conn
  [sql|
    select
      gateway_notification_id
    , creation_time
    , all_params
    , raw_path
    , raw_query_string
    , notification_type
    , gateway_connection
    , task_status
    , task_result
    , task_last_updated_time
    from gateway_notifications
    where gateway_notification_id = ?
    limit ?
    |]
    (gatewayNotificationId, 1 :: Int)

data NotificationType = SubscriptionNotification | BillingNotification | UnsubscriptionNotification
  deriving (Show, Read, Eq, Generic, Enum, Bounded)
instance A.ToJSON NotificationType
instance A.FromJSON NotificationType
instance Scotty.Parsable NotificationType where
    parseParam txt =
      let dic = M.fromList $ map (\g -> (replace "notification" "" . toLower . pack . show $ g, g)) [minBound ..]
      in case M.lookup (toLower txt) dic of
        Just gw -> Right gw
        Nothing -> Left $ concat ["Unable to parse ", txt, " to a NotificationType"]


data AsyncTaskStatus = NotStarted | Started | Completed | Failed
  deriving (Show, Read, Eq, Generic, Enum)
instance A.ToJSON AsyncTaskStatus
instance A.FromJSON AsyncTaskStatus

-- | GatewayNotification
-- Example:
--
-- > get "/notification" $ do
-- >   req <- request
-- >   allParams <- params
-- >   let notification = makeGatewayNotification
-- >         PayGuruStandard
-- >         SubscriptionNotification
-- >         req
-- >         allParams
-- >   res <- tryQuery (addGatewayNotification notification)
-- >   (text . pack . show ||| json) res
data GatewayNotification = GatewayNotification {
    gatewayNotificationId :: Int
  , creationTime :: Time.ZonedTime
  , allParams :: M.Map Text Text
  , rawPath :: Text
  , rawQueryString :: Text
  , notificationType :: NotificationType
  , gatewayConnection :: GatewayConnection
  , taskStatus :: AsyncTaskStatus
  , taskResult :: Maybe Text
  , taskLastUpdatedTime :: Maybe Time.ZonedTime
} deriving (Show, Generic)

instance PS.ToRow GatewayNotification where
  toRow d = [
    --   toField (gatewayNotificationId d)
    --   toField (creationTime d)
        toField (allParams d)
      , toField (rawPath d)
      , toField (rawQueryString d)
      , toField (notificationType d)
      , toField (gatewayConnection d)
      , toField (taskStatus d)
      , toField (taskResult d)
      , toField (taskLastUpdatedTime d)
    ]
instance PS.FromRow GatewayNotification

instance A.ToJSON GatewayNotification
instance A.FromJSON GatewayNotification
