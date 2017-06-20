{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , OverloadedStrings
#-}

module WebServer (
  main
) where

import Control.Monad.Trans.Reader (ReaderT(..), runReaderT)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader)
-- import qualified Data.Map as M
import Data.Text.Lazy (Text, pack, unpack)
-- import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified System.Environment as Env
import qualified Data.ByteString.Char8 as Char8
import Web.Scotty.Trans
import Network.Wai.Middleware.RequestLogger
-- import qualified Data.Aeson as A
-- import Debug.Trace (trace)
import PostgreSQLConnectionPool (getAllVisits, myPool)
import qualified Data.Pool as P

type AppState = ()

newtype WebM a = WebM { runWebM :: ReaderT  AppState IO a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppState)

app :: ScottyT Text WebM ()
app = do
  middleware logStdoutDev

  post "/" $ do
    -- let list = A.decode b :: Maybe [Int]
    list <- jsonData :: ActionT Text WebM [Int]
    text $ ( pack . show . sum)  list

  get "/visits" $ do
      connectionString <- liftIO $ Env.getEnv "SCOTCH_DB"
      pool <- liftIO $ myPool (Char8.pack connectionString)
      visits <- liftIO $ P.withResource pool getAllVisits
      text $ pack (show visits)

    -- let list = A.decode b :: Maybe [Int]
    -- list <- jsonData :: ActionT Text WebM [Int]
    -- text $ ( pack . show . sum)  list


  -- list all connections



main :: IO ()
main = do
  let runActionToIO m = runReaderT (runWebM m) ()
  scottyT 3000 runActionToIO app
