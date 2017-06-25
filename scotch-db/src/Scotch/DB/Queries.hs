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
)
where

import qualified Database.PostgreSQL.Simple as PS
import Database.PostgreSQL.Simple.SqlQQ
import Scotch.DB.Types

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
