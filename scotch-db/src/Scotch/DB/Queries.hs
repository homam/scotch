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
  , updateANotification
)
where

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
    (taskStatus n, taskResult n, gatewayNotificationId n)
  )
