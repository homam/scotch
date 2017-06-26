{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , OverloadedStrings
  , RankNTypes
  , FlexibleContexts
  , NamedFieldPuns
  , ScopedTypeVariables
#-}

module WebServer (
  main
) where

import Control.Applicative ((<|>))
import Control.Monad.Trans.Reader (ReaderT(..), runReaderT)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, lift)
-- import qualified Data.Map as M
import Data.Text.Lazy (Text, pack)
import qualified System.Environment as Env
import qualified Data.ByteString.Char8 as Char8
import Web.Scotty.Trans hiding (status)
import qualified Web.Scotty.Trans as Trans
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import qualified Network.Wai as Wai
-- import qualified Data.Aeson as A
-- import Debug.Trace (trace)
import qualified Data.Pool as P
import Control.Arrow ((|||), (***))
import Scotch.DB.Types (Postback(..))
import Scotch.DB.Queries (getAllVisits, addVisit, addPostback)
import Scotch.DB.QueryHelpers (myPool, QueryRunner, runQuery, tryRunQuery)

newtype AppState = AppState {
  _query :: QueryRunner IO IO
}

query :: QueryRunner IO (ActionT Text WebM)
query = runQuery lift _query

tryQuery = tryRunQuery _query


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
    hs <- Trans.headers
    json (Char8.unpack rawPath, qs , hs)


  get "/visits" $ do
    visits <- query getAllVisits
    text $ pack (show visits)

    -- let list = A.decode b :: Maybe [Int]
    -- list <- jsonData :: ActionT Text WebM [Int]
    -- text $ ( pack . show . sum)  list

  get "/postback" $ do
    pst <- Postback <$> pure 0 <*> param "transactionid" <*> param "subsid" <*> param "service" <*> (param "status" <|> pure 0)

    ps <- tryQuery (addPostback pst)

    text $ (pack . show ||| pack . const "Ok") ps



main :: IO ()
main = do
  connectionString <- liftIO $ Env.getEnv "SCOTCH_DB"
  pool <- myPool (Char8.pack connectionString)
  let appState = AppState { _query = P.withResource pool }
  let runActionToIO m = runReaderT (runWebM m) appState
  scottyT 3000 runActionToIO app
