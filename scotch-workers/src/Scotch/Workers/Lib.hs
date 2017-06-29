module Scotch.Workers.Lib
    ( someFunc
    ) where

import qualified System.Environment as Env
import qualified Data.ByteString.Char8 as Char8
import Control.Monad.Trans (liftIO)
import Control.Monad (forever)
import qualified Data.Pool as P
import qualified Control.Concurrent as C
import qualified Control.Concurrent.Async as Async
import qualified Scotch.DB.Queries as Q
import qualified Scotch.DB.Types.GatewayNotification as N
import Scotch.DB.QueryHelpers (myPool)

processNotification :: N.GatewayNotification -> IO N.GatewayNotification
processNotification notification = return $ notification { N.taskStatus = N.NotStarted }

someFunc :: IO ()
someFunc = do
  connectionString <- liftIO $ Env.getEnv "SCOTCH_DB"
  pool <- myPool (Char8.pack connectionString)
  let query = P.withResource pool
  forever $
    query (Q.getAllNotifications N.NotStarted N.SubscriptionNotification 10)
    >>= Async.mapConcurrently processNotification
    >>= Async.mapConcurrently_ (query . Q.updateANotification)
    >> C.threadDelay (1*10^6)
