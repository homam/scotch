{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric
#-}
module PostgreSQLConnectionPool (
  main
) where

import qualified Data.Pool as P
import Control.Monad (forM_)
import qualified Control.Concurrent as C
-- import qualified Database.PostgreSQL.LibPQ as PQ
import qualified Database.PostgreSQL.Simple as PS
import qualified Database.PostgreSQL.Simple.ToRow (toRow)
import Database.PostgreSQL.Simple.ToField (toField)
import qualified Control.Exception as X
import Control.Arrow ((|||))
import qualified Control.Concurrent.STM as STM
import qualified GHC.Int
import qualified Data.Aeson as A
import qualified Data.Map as M
import GHC.Generics (Generic)

-- utility function to just print the current number of open connections
connectionCount :: IO ()
connectionCount = do
    conn <- PS.connectPostgreSQL "postgres://postgres:ubuntu14@95.97.146.246/postgres"
    x <- PS.query_ conn "SELECT COUNT (*) FROM pg_stat_activity"
    let c = PS.fromOnly . head $ (x :: [PS.Only Int]) -- cast result to typed haskell
    PS.close conn
    putStrLn $ "-- Connections Count = " ++ show c

-- our SQL query functions will look like this, taking a connection and returning IO result
randomInt :: PS.Connection -> IO Int
randomInt conn = do
  x <- PS.query_ conn "select round(100 * random() * random() ) :: Int"
  let films = map PS.fromOnly (x :: [PS.Only Int]) -- cast result to typed haskell
  return $ head films


addVisit :: PS.Connection -> IO GHC.Int.Int64
addVisit conn = PS.execute_ conn
   "insert into visits (campaign_id, landing_page_id, ip, ip_country, headers, query_params) VALUES (1, 1, '127.0.0.1'::inet, '--', null, null);"

data Visit = Visit {
    campaign_id :: Int
  , landing_page_id :: Int
  , ip :: String
  , ip_country :: String
  , headers :: M.Map String String
  , query_params :: M.Map String String
} deriving (Show, Generic)

instance PS.ToRow Visit where
  toRow d = [
      toField (campaign_id d)
    , toField (landing_page_id d)
    , toField (ip d)
    , toField (ip_country d)
    , toField (A.toJSON $ headers d)
    , toField (A.toJSON $ query_params d)
    ]

addVisit' :: Visit -> PS.Connection -> IO GHC.Int.Int64
addVisit' v conn = PS.executeMany conn
  "insert into visits (campaign_id, landing_page_id, ip, ip_country, headers, query_params) VALUES (?, ?, ?, ?, ?, ?);"
  [v]


-- create a connection pool
-- reference: http://codeundreamedof.blogspot.nl/2015/01/a-connection-pool-for-postgresql-in.html
myPool :: IO (P.Pool PS.Connection)
myPool = P.createPool (PS.connectPostgreSQL "postgres://postgres:ubuntu14@95.97.146.246/postgres") PS.close 1 10 10

test :: P.Pool PS.Connection -> IO ()
test pool = do
  connectionCount
  result <- STM.atomically $ STM.newTVar ([] :: [Int])
  errors <- STM.atomically $ STM.newTVar ([] :: [X.SomeException])
  let x = C.forkIO $ X.try (P.withResource pool randomInt)
          >>= (prepend errors ||| prepend result)
  forM_ [1 .. 20] $ const x -- run x 20 times in parallel for fun
  wait 1
  connectionCount
  wait 3 -- wait 3 seconds for querirs to finish
  result' <- STM.atomically $ STM.readTVar result
  putStrLn $ "== Result = " ++ show (sum result')
  errors' <- STM.atomically $ STM.readTVar errors
  mapM_ print errors' -- print errors (if any)
  where prepend m = STM.atomically . STM.modifyTVar m . (:)


-- instance (A.ToJSON a) => A.ToJSON (M.Map a b)
--     where toJSON kvs = object [ t .= v | (k,v) <- kvs, let (A.String t) = toJSON k ]


main :: IO ()
main = do
    pool <- myPool
    -- P.withResource pool addVisit
    P.withResource pool (addVisit' $ Visit 1 1 "127.0.0.1" "--" (M.fromList [("A", "B")]) (M.fromList []))
    return ()
    -- test pool
    -- wait 15 -- our pool should have expired in 10 seconds, but it reconnects
    -- test pool
    -- P.destroyAllResources pool

wait :: Int -> IO ()
wait s = C.threadDelay (10^6 * s) -- utility function, wait in seconds
