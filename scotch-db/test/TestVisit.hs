{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestVisit (
  main
) where

import           Control.Monad                       (msum)
import qualified Data.Map                            as M
import           Data.Monoid                         ((<>))
import qualified Data.Pool                           as P
import qualified Data.Time                           as Time
import qualified Network.Wai                         as Wai
import qualified Network.Wai.Internal                as WaiI
import qualified Scotch.DB.Gateways                  as G
import           Scotch.DB.QueryHelpers              (defaultTime, myPool)
import           Scotch.DB.Types                     (CampaignId (..),
                                                      LandingPage (..),
                                                      OptInMethod (..))
import           Scotch.DB.Types.GatewayConnection
import qualified Scotch.DB.Types.GatewayNotification as N
import qualified Scotch.DB.Types.Visit               as V
import qualified Scotch.DB.Types.PixelValue as PV
import Scotch.DB.Types.Affiliate (AffiliateId (..), Affiliate (..), standardAffiliate, HandsetLevel (..), PixelValueUrlRepresentation (..))
import Scotch.DB.Types.GatewayOperator
import Scotch.DB.Types.Imports
import qualified System.Environment                  as Env
-- import qualified Web.Scotty.Internal.Types as ST
import           Control.Arrow                       ((***))
import           Control.Monad.Trans.Except
import qualified Data.ByteString.Char8               as Char8
import qualified Data.Text                           as T
import qualified Data.Text.Encoding                  as Encoding
import           Data.Text.Lazy                      (fromStrict, toStrict)

toQueryString = map (T.split (=='=')) . T.split (=='&')
toTuple (k:v:_) = (k, v)
toTuple (k:_)   = (k, "")
toTuple []      = ("", "")

addVisit pool = do
  let rawQueryString = "?click_id=123456&Pub_Id=com.google"
  let queryParams = queryStringFromMap $ M.fromList $ map toTuple $ toQueryString $ toStrict rawQueryString
  let rawPath = "/visit/1/PayguruTurkey/SomePage/" <> rawQueryString
  let gatewayConnection = Just PayguruTurkey
  let campaignId = CampaignId 1
  let landingPage = LandingPage "SomePage"
  let headers = M.fromList [("remote-address", "127.0.0.1"), ("user-agent", "homam client")]
  let ipCountry = Nothing
  let optInMethod = RedirectToPaymentPage
  (newVisitId :: Int, newCreationTime :: Time.ZonedTime) <- P.withResource pool $
    V.insertVisit V.Visit {
        V.visitId = 0 -- auto generated value
      , V.creationTime = defaultTime -- auto generated value
      , V.queryParams = queryParams
      , V.rawQueryString = rawQueryString
      , V.rawPath = rawPath
      , V.gatewayConnection = gatewayConnection
      , V.campaignId = campaignId
      , V.landingPage = landingPage
      , V.ip = msum $ map (`M.lookup` headers) ["remote-address"]
      , V.ipCountry = ipCountry
      , V.headers = headers
      , V.optInMethod = optInMethod
    }
  return (newVisitId, newCreationTime)

{--
  http://paymentmerchant.com/SubscriptionNotification?service=xxServicexx&subsId=xxSubsIdxx&status=xxStatusxx&oId=xxOidxx&transactionId=xxTransactionIdxx&xxQSxx
  let queryString = "service=xxServicexx&subsId=xxSubsIdxx&status=xxStatusxx&oId=xxOidxx&transactionId=xxTransactionIdxx&xxQSxx"
  map (T.split (=='=')) $ T.split (=='&') queryString
--}
addNotification pool visitId = do
  let queryString = "service=2616&subsId=sid123456&status=active&oId=5&transactionId=2172613&sctvid=" <> Encoding.encodeUtf8 (T.pack (show visitId))
  let queryString' = toQueryString (Encoding.decodeUtf8 queryString)
  let request = Wai.defaultRequest {
      Wai.rawPathInfo = "/notification/subscription/PayguruTurkey"
    , Wai.rawQueryString = queryString
    , WaiI.queryString = map ((Encoding.encodeUtf8 *** fmap Encoding.encodeUtf8) . toTuple') queryString'
    }
  let allParams = [("notificationType", "subscription"), ("gateway", "PayguruTurkey")] ++ map ((fromStrict *** fromStrict) . toTuple) queryString'
  let noti = N.makeGatewayNotification request PayguruTurkey N.SubscriptionNotification allParams
  P.withResource pool $ N.insertGatewayNotification noti
  where
    toTuple' (k:v:_) = (k, Just v)
    toTuple' (k:_)   = (k, Nothing)
    toTuple' []      = ("", Nothing)

-- {--
processNotification pool notificationId = do
  mnoti <- P.withResource pool $ N.getOneNotification notificationId

  let campaignIdMap = M.fromList [
        (CampaignId 1, standardAffiliate (AffiliateId "BillyMob") "http://pixel.billymob.com/?clickid={click_id}&v={pixle_value}")
        ]
  -- get 'pixelMap' from a database.
  -- It maps certain parameters to a pixel value
  let  pixelMap = M.fromList [
          ((PayguruTurkey, Nothing, Nothing, Just HighEnd), Just (PV.PixelValueId 0, 0.8 , NoPixelValueUrlRepresentation))
        , ((PayguruTurkey, Just TR_AVEA, Just (AffiliateId "BillyMob"), Just HighEnd), Just (PV.PixelValueId 0, 0.5 , NoPixelValueUrlRepresentation))
        ]
  -- create an instance of readonly configuration using values retrieved from the databae
  let config = PV.PixelValueUrlRepresentationConfig { PV.campaignIdMap = campaignIdMap, PV.pixelMap = pixelMap }
  case mnoti of
    Nothing -> putStrLn "No Notification was Found!"
    Just noti -> do
      let gateway = G.getGateway (N.gatewayConnection noti)
      pixelResult <- G.runProcessNotification (P.withResource pool $ G.processNotification gateway noti) config
      case pixelResult of
        Left  e -> putStrLn $ T.unpack e
        Right ((), logs) -> mapM_ (putStrLn . T.unpack) logs
--}

main = do
  putStrLn "starting..."
  connectionString <- Env.getEnv "SCOTCH_DB"
  pool <- myPool (Char8.pack connectionString)
  (visitId, _) <- addVisit pool
  putStrLn $ "Visit added " ++ show visitId

  (notificationId, _) <- addNotification pool visitId
  putStrLn $ "Notification added " ++ show notificationId

  processNotification pool notificationId
