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
  , addPostback
  , addGatewayNotification
  , getAllNotifications
)
where

import qualified Data.Time as Time
import qualified Database.PostgreSQL.Simple as PS
import Database.PostgreSQL.Simple.SqlQQ
import Scotch.DB.Types
import Scotch.DB.Types.GatewayNotification

getAllVisits :: PS.Connection -> IO [Visit]
getAllVisits conn = PS.query_ conn [sql|
  select visit_id, creation_time, campaign_id, landing_page_id, text(ip) as ip, ip_country, headers, query_params
  from visits order by visit_id desc limit 10; |]


addVisit :: (PS.FromRow r, PS.ToRow q) => q -> PS.Connection -> IO [r]
addVisit v conn = PS.query
  conn
   [sql|
    insert into visits (campaign_id, landing_page_id, ip, ip_country, headers, query_params)
    VALUES (?, ?, ?, ?, ?, ?) returning visit_id, creation_time; |]
  v

addPostback :: PS.ToRow q => q -> PS.Connection -> IO [(Int, Int)]
addPostback pst conn = PS.query
  conn
  [sql|
    insert into integration_payguru_billings (transactionid, subsid, service, status)
    VALUES (?, ?, ?, ?) returning integration_payguru_billing_id, 0; |]
  pst

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
    from gateway_notifications
    where task_status = ?
      and notification_type = ?
    order by gateway_notification_id desc
    limit ?
    |]
  (tstatus, ntype, limit)
