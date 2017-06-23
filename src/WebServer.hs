{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , OverloadedStrings
  , RankNTypes
  , FlexibleContexts
#-}

module WebServer (
  main
) where

import Control.Monad.Trans.Reader (ReaderT(..), runReaderT)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, lift)
-- import qualified Data.Map as M
import Data.Text.Lazy (Text, pack, unpack)
-- import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified System.Environment as Env
import qualified Data.ByteString.Char8 as Char8
import Web.Scotty.Trans
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import qualified Network.Wai as Wai
import qualified Data.Aeson as A
-- import Debug.Trace (trace)
import PostgreSQLConnectionPool (getAllVisits, myPool, QueryRunner, runQuery)
import qualified Data.Pool as P
import Control.Arrow ((***))

newtype AppState = AppState {
  _query :: QueryRunner IO IO
}

query :: QueryRunner IO (ActionT Text WebM)
query = runQuery lift _query

newtype WebM a = WebM { runWebM :: ReaderT AppState IO a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppState)

app :: ScottyT Text WebM ()
app = do
  middleware logStdoutDev

  get "/" $ do
    req <- request
    let rawPath = Wai.rawPathInfo req
    let rawQueryString = Wai.rawQueryString req
    let qs = map (Char8.unpack *** fmap Char8.unpack) $ Wai.queryString req
    hs <- headers
    json (Char8.unpack rawPath, qs , hs)


  post "/" $ do
    -- b <- body
    -- let list = A.decode b :: Maybe [Int]
    list <- jsonData :: ActionT Text WebM [Int]
    text $ ( pack . show . sum)  list

  get "/visits" $ do
    visits <- query getAllVisits
    text $ pack (show visits)

    -- let list = A.decode b :: Maybe [Int]
    -- list <- jsonData :: ActionT Text WebM [Int]
    -- text $ ( pack . show . sum)  list


  -- list all connections



main :: IO ()
main = do
  connectionString <- liftIO $ Env.getEnv "SCOTCH_DB"
  pool <- myPool (Char8.pack connectionString)
  let appState = AppState { _query = P.withResource pool }
  let runActionToIO m = runReaderT (runWebM m) appState
  scottyT 3000 runActionToIO app
