{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , OverloadedStrings
  , RankNTypes
  , FlexibleContexts
  , NamedFieldPuns
  , ScopedTypeVariables
  , MultiParamTypeClasses
  , AllowAmbiguousTypes
#-}

module WebServer (
  main
) where

import Control.Applicative ((<|>))
import Control.Monad.Trans.Reader (ReaderT(..), runReaderT)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, lift)
import qualified Data.Map as M
import Data.Text.Lazy (Text, pack)
import qualified Data.Text.Lazy as Text
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
import Scotch.DB.Queries (getAllVisits, addVisit, addGatewayNotification)
import Scotch.DB.QueryHelpers (myPool, QueryRunner, runQuery, tryRunQuery)
import Scotch.DB.Types.GatewayNotification
import Scotch.DB.Types.GatewayConnection
import qualified Scotch.DB.Types.Visit as Visit
import Scotch.DB.Types (LandingPage(..))
import Scotch.DB.Gateways (IsGateway(..), AllGateways(..))
import Scotch.DB.Gateways.PayGuruGateway
import Scotch.DB.Gateways.TestGateway

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

  get "/visit/:campaignId/:gateway/:landingpage" $ do
    gateway :: GatewayConnection <- param "gateway"
    landingPage <- LandingPage <$> param "landingpage"
    optInMethod <- handleVisit (getGateway gateway) landingPage

    visit <- Visit.makeVisit (Just gateway) <$> (Visit.CampaignId <$> param "campaignId") <*> pure landingPage <*> request <*> pure optInMethod

    -- liftIO $ putStrLn "hello"
    res <- tryQuery (addVisit visit)

    liftIO $ print res

    -- liftIO $ print res

    return ()


  get "/notification/:notificationType/:gateway" $ do
    req <- request
    allParams <- params
    notificationType :: NotificationType <- param "notificationType"
    gateway :: GatewayConnection <- param "gateway"

    let notification = makeGatewayNotification
          gateway
          notificationType
          req
          allParams
    res <- tryQuery (addGatewayNotification notification)

    text $ (pack . show) notification


main :: IO ()
main = do
  connectionString <- liftIO $ Env.getEnv "SCOTCH_DB"
  pool <- myPool (Char8.pack connectionString)
  let appState = AppState { _query = P.withResource pool }
  let runActionToIO m = runReaderT (runWebM m) appState
  scottyT 3000 runActionToIO app


getGateway :: GatewayConnection -> AllGateways
getGateway PayguruTurkey = PayGuru PayGuruGateway { payGuruEndPointUrl = "http://.com", payGuruUsername = "pguname" }
getGateway TestStandard = Test TestGateway

-- newtype LandingPage = LandingPage Text

-- getFlow :: GatewayNotification.GatewayConnection -> LandingPage -> ActionT Text WebM ()
-- getFlow GatewayNotification.PayGuruStandard (LandingPage lp) = do
--     req <- request
--     let ip = Wai.remoteHost req
--     -- redirect $ Text.concat ["http://www.something.com/", lp]
--     raise "Some error"
