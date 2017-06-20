{-# LANGUAGE
    OverloadedStrings
#-}
module PostgreSQLConnectionPool (
  main
) where

import qualified Data.Pool as P
import Control.Monad (forM_)
import qualified Control.Concurrent as C
-- import qualified Database.PostgreSQL.LibPQ as PQ
import qualified Database.PostgreSQL.Simple as PS
import qualified Control.Exception as X
import Control.Arrow ((|||))
import qualified Control.Concurrent.STM as STM

-- utility function to just print the current number of open connections
connectionCount :: IO ()
connectionCount = do
    conn <- PS.connectPostgreSQL ""
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

-- create a connection pool
-- reference: http://codeundreamedof.blogspot.nl/2015/01/a-connection-pool-for-postgresql-in.html
myPool :: IO (P.Pool PS.Connection)
myPool = P.createPool (PS.connectPostgreSQL "") PS.close 1 10 10

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

main :: IO ()
main = do
    pool <- myPool
    test pool
    wait 15 -- our pool should have expired in 10 seconds, but it reconnects
    test pool
    P.destroyAllResources pool

wait :: Int -> IO ()
wait s = C.threadDelay (10^6 * s) -- utility function, wait in seconds
