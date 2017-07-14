{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
  , FlexibleInstances
  , ScopedTypeVariables
  , RankNTypes
#-}
module Scotch.DB.QueryHelpers (
    myPool
  , QueryRunner
  , runQuery
  , tryRunQuery
  , defaultTime
  , safeHead
) where

import qualified Data.Pool as P
import qualified Database.PostgreSQL.Simple as PS
import qualified Data.ByteString as BS
import Control.Monad.Trans (MonadIO, liftIO, MonadTrans)
import Control.Monad.Reader (MonadReader, ask, lift)
import Control.Exception (try, SomeException)
import qualified Data.Time as Time



-- | Helper type synonym for specifying a monadic transformation between 'm' and 'n' monads. For example:
-- > QueryRunner IO IO
-- > QueryRunner IO (EitherT m IO)
type QueryRunner m n = (Monad m, Monad n) => forall a. (PS.Connection -> m a) -> n a

-- | Helper function for running 'QueryRunner m n' instances that are stored in Scotty reader config. Usage example:
-- > query :: QueryRunner IO (ActionT Text WebM)
-- > query = runQuery lift _query
runQuery :: (MonadIO n, MonadReader r m) => (m r -> n a) -> (a -> t -> IO b) -> t -> n b
runQuery lift query task = do
  q <- query <$> lift ask
  liftIO $ q task

tryRunQuery :: (MonadIO (t1 m), MonadReader a1 m, MonadTrans t1) => (a1 -> t -> IO a) -> t -> t1 m (Either SomeException a)
tryRunQuery query task = do
  runner <- query <$> lift ask
  liftIO $ try (runner task)


-- | Create a connection pool
-- reference: http://codeundreamedof.blogspot.nl/2015/01/a-connection-pool-for-postgresql-in.html
myPool :: BS.ByteString -> IO (P.Pool PS.Connection)
myPool connectionString = P.createPool (PS.connectPostgreSQL connectionString) PS.close 1 10 10


defaultTime :: Time.ZonedTime
defaultTime = Time.utcToZonedTime (Time.minutesToTimeZone 120) $ Time.UTCTime (Time.fromGregorian 2017 1 1) 0

safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead [] = Nothing

-- Usage example:
-- > main = do
-- >   connectionString <- Env.getEnv "SCOTCH_DB"
-- >   pool <- myPool (Char8.pack connectionString)
-- >   [(newVisitId :: Int, newCreationTime :: Time.ZonedTime)] <- P.withResource pool $ addVisit Visit {
-- >       visit_id = 0
-- >     , creation_time = Time.utcToZonedTime (Time.minutesToTimeZone 120) $ UTCTime (Time.fromGregorian 2017 1 1) 0
-- >     , campaign_id = 1
-- >     , landing_page_id = 1
-- >     , ip = "127.0.0.1"
-- >     , ip_country = ""
-- >     , headers = Nothing
-- >     , query_params = Nothing
-- >   }
-- >   print $ "Record Inserted with " ++ show newVisitId ++ " at " ++ show newCreationTime
-- >   visits <- P.withResource pool getAllVisits
-- >   print $ A.encode visits
