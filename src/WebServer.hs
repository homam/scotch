{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , OverloadedStrings
  , NamedFieldPuns
  , ScopedTypeVariables
#-}

module WebServer (
  main
) where

import Control.Applicative ((<|>))
import Control.Monad.Trans.Reader (ReaderT(..), runReaderT)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader)
-- import qualified Data.Map as M
import Data.Text.Lazy (Text, pack, unpack)
-- import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified System.Environment as Env
import qualified Data.ByteString.Char8 as Char8
import Web.Scotty.Trans hiding (status)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import qualified Network.Wai as Wai
import qualified Data.Aeson as A
-- import Debug.Trace (trace)
import PostgreSQLConnectionPool (getAllVisits, myPool, addPostback)
import qualified Data.Pool as P
import Control.Arrow ((***), (|||))
import Data.Maybe (fromMaybe)
import Types (Postback (..))
import Control.Exception (try, SomeException)

type AppState = ()

newtype WebM a = WebM { runWebM :: ReaderT  AppState IO a }
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
      connectionString <- liftIO $ Env.getEnv "SCOTCH_DB"
      pool <- liftIO $ myPool (Char8.pack connectionString)
      visits <- liftIO $ P.withResource pool getAllVisits
      text $ pack (show visits)

    -- let list = A.decode b :: Maybe [Int]
    -- list <- jsonData :: ActionT Text WebM [Int]
    -- text $ ( pack . show . sum)  list

  get "/postback" $ do
    params <- params
    pst <- Postback <$> pure 0 <*> param "transactionid" <*> param "subsid" <*> param "service" <*> (param "status" <|> pure 0)

    connectionString <- liftIO $ Env.getEnv "SCOTCH_DB"
    pool <- liftIO $ myPool (Char8.pack connectionString)

    ps :: (Either SomeException [(Int, Int)]) <- liftIO $ try (P.withResource pool (addPostback pst))

    text $ (pack . show ||| pack . show . head) ps


  -- list all connections



main :: IO ()
main = do
  let runActionToIO m = runReaderT (runWebM m) ()
  scottyT 3000 runActionToIO app
