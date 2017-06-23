{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric
  , NamedFieldPuns
  , FlexibleInstances
  , ScopedTypeVariables
  , RankNTypes
#-}
module PostgreSQLConnectionPool (
    getAllVisits
  , addVisit
  , myPool
  , Visit (..)
  , QueryRunner
  , runQuery
  , tryRunQuery
  , addPostback
) where

import Data.Maybe (fromMaybe)
import qualified Data.Pool as P
import qualified Database.PostgreSQL.Simple as PS
import Database.PostgreSQL.Simple.ToRow (toRow)
import Database.PostgreSQL.Simple.ToField (toField, ToField)
import Database.PostgreSQL.Simple.FromField (fromField, FromField)
import qualified Data.ByteString.Lazy as BL
import Data.Time.Clock (UTCTime(..))
import qualified Data.Time as Time
import qualified Data.Aeson as A
import qualified Data.Map as M
import qualified System.Environment as Env
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import GHC.Generics (Generic)
import Control.Monad.Trans (MonadIO, liftIO, MonadTrans)
import Control.Monad.Reader (MonadReader, ask, lift)
import Control.Exception (try, SomeException)
import Types (Postback (..))

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

instance ToField (M.Map String String) where
  toField = toField . A.toJSON

instance FromField (M.Map String String) where
  fromField _ Nothing = return M.empty
  fromField _ (Just bs) = return $ fromMaybe M.empty (A.decode $ BL.fromStrict bs)

getAllVisits :: PS.Connection -> IO [Visit]
getAllVisits conn = PS.query_ conn "select visit_id, creation_time, campaign_id, landing_page_id, text(ip) as ip, ip_country, headers, query_params from visits order by visit_id desc limit 10;"

data Visit = Visit {
    visit_id :: Int
  , creation_time :: Time.ZonedTime
  , campaign_id :: Int
  , landing_page_id :: Int
  , ip :: String
  , ip_country :: String
  , headers :: Maybe (M.Map String String)
  , query_params :: Maybe (M.Map String String)
} deriving (Show, Generic)

instance PS.ToRow Visit where
  toRow d = [
      -- visit_id -- auto increamenting
      -- creation_time -- auto
      toField (campaign_id d)
    , toField (landing_page_id d)
    , toField (ip d)
    , toField (ip_country d)
    , toField (headers d)
    , toField (query_params d)
    ]
instance PS.FromRow Visit

-- we want to be able to A.decode visits
instance A.ToJSON Visit
instance A.FromJSON Visit


addVisit :: (PS.FromRow r, PS.ToRow q) => q -> PS.Connection -> IO [r]
addVisit v conn = PS.query
  conn
  "insert into visits (campaign_id, landing_page_id, ip, ip_country, headers, query_params) VALUES (?, ?, ?, ?, ?, ?) returning visit_id, creation_time;"
  v

addPostback :: PS.ToRow q => q -> PS.Connection -> IO [(Int, Int)]
addPostback pst conn = PS.query
  conn
  "insert into integration_payguru_billings (transactionid, subsid, service, status) VALUES (?, ?, ?, ?) returning integration_payguru_billing_id, 0;"
  pst

-- create a connection pool
-- reference: http://codeundreamedof.blogspot.nl/2015/01/a-connection-pool-for-postgresql-in.html
myPool :: BS.ByteString -> IO (P.Pool PS.Connection)
myPool connectionString = P.createPool (PS.connectPostgreSQL connectionString) PS.close 1 10 10

main = do
  connectionString <- Env.getEnv "SCOTCH_DB"
  pool <- myPool (Char8.pack connectionString)
  [(newVisitId :: Int, newCreationTime :: Time.ZonedTime)] <- P.withResource pool $ addVisit Visit {
      visit_id = 0
    , creation_time = Time.utcToZonedTime (Time.minutesToTimeZone 120) $ UTCTime (Time.fromGregorian 2017 1 1) 0
    , campaign_id = 1
    , landing_page_id = 1
    , ip = "127.0.0.1"
    , ip_country = ""
    , headers = Nothing
    , query_params = Nothing
  }
  print $ "Record Inserted with " ++ show newVisitId ++ " at " ++ show newCreationTime
  visits <- P.withResource pool getAllVisits
  print $ A.encode visits
