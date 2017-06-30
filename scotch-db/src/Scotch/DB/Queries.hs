{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
  , FlexibleInstances
  , ScopedTypeVariables
  , RankNTypes
  , QuasiQuotes
#-}

module Scotch.DB.Queries (
    getAllVisits
  , addVisit
  , addGatewayNotification
  , getAllNotifications
  , updateANotification
)
where

import qualified Data.Time as Time
import qualified Database.PostgreSQL.Simple as PS
import Database.PostgreSQL.Simple.SqlQQ
import qualified Scotch.DB.Types.Visit as Visit
import Scotch.DB.Types.GatewayNotification

getAllVisits :: PS.Connection -> IO [Visit.Visit]
getAllVisits conn = PS.query_ conn [sql|
  select
    visit_id
  , creation_time
  , campaign_id
  , landing_page
  , text(ip) as ip
  , ip_country
  , headers
  , query_params
  , raw_path
  , raw_query_string
  , gateway_connection
  , opt_in_method
  from visits order by visit_id desc limit 10; |]


addVisit :: Visit.Visit -> PS.Connection -> IO (Int, Time.ZonedTime)
addVisit v conn = head <$> PS.query
  conn
   [sql|
    insert into visits (
      campaign_id
    , landing_page
    , ip
    , ip_country
    , headers
    , query_params
    , raw_path
    , raw_query_string
    , gateway_connection
    , opt_in_method
    )
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    returning visit_id, creation_time; |]
  v

addGatewayNotification :: GatewayNotification -> PS.Connection -> IO (Int, Time.ZonedTime)
addGatewayNotification notification conn = head <$> PS.query
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
    )
    VALUES (?, ?, ?, ?, ?, ?, ?) returning gateway_notification_id, creation_time; |]
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

updateANotification :: GatewayNotification -> PS.Connection -> IO ()
updateANotification n conn = const () <$> (PS.execute
    conn
    [sql|
      update gateway_notifications SET
        task_status = ?
      , task_result = ?
      , task_last_updated_time = now()
      where gateway_notification_id = ?
      |]
    (taskStatus n, taskResult n, gateway_notification_id n)
  )
