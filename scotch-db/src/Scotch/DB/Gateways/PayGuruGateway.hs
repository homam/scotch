{-# LANGUAGE
    OverloadedStrings
#-}

module Scotch.DB.Gateways.PayGuruGateway (
  PayGuruGateway(..)
)
where

import Scotch.DB.IsGateway
import Scotch.DB.Types
import qualified Scotch.DB.Types.Sale as S
import qualified Scotch.DB.Types.Visit as V
import qualified Scotch.DB.Types.Pixel as PX
import qualified Scotch.DB.Types.PixelValue as PV
import qualified Scotch.DB.Types.GatewayOperator as O
import qualified Scotch.DB.Types.GatewayNotification as N
import qualified Scotch.DB.Types.GatewayService as GS
import Scotch.DB.QueryHelpers (defaultTime)
import Data.Text.Lazy (unpack, pack, fromStrict)
import qualified Data.Text as T
import Data.Monoid ((<>))
import Web.Scotty.Trans
import Scotch.DB.Types.GatewayNotification as GatewayNotification
import qualified Data.Map as M
import Control.Monad.Trans.Except
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad (void)
import Text.Read (readMaybe)
import qualified Data.CaseInsensitive as CI

data PayGuruGateway = PayGuruGateway {
      payGuruEndPointUrl :: String
    , payGuruUsername :: String
  }

instance IsGateway PayGuruGateway where
    handleVisit _ _ = text "pay flow" >> return RedirectToPaymentPage
    processNotification _ noti conn = case notificationType noti of
        SubscriptionNotification -> processSubscriptionNotification noti conn
        BillingNotification -> error "Not implemented"
        s -> throwPN $ "Unexpected GatewayNotification type " <> T.pack (show s)
    identifier _ = GatewayNotification.PayguruTurkey


processSubscriptionNotification noti conn = do
  let ciParams = M.mapKeys CI.mk (N.allParams noti)
  let visitId = readMaybe . unpack =<< M.lookup (CI.mk "sctvid") ciParams
  let gatewayConnection = N.gatewayConnection noti
  case getParams ciParams of
    Nothing -> throwPN "Operator Not Found"
    Just (operator, service) -> do
      -- add sales
      (salesId, _) <- tryPN $ S.insertSale S.Sale {
          S.salesId = 0
        , S.creationTime = defaultTime
        , S.gatewayConnection = gatewayConnection
        , S.gatewayOperator = operator
        , S.service = service
        , S.isActive = True
        , S.unsubscriptionTime = Nothing
        , S.visitId = visitId
        } conn

      tellPN $ "Sale inserted: " <> T.pack (show salesId)

      liftIO $ putStrLn ">> SALE ADDED"


      case visitId of
          Nothing    -> tellPN "Cannot insert a pixel becasue visitId is null"
          (Just vid) -> do

            mvisit <- tryPN $ V.getOneVisit vid conn

            case mvisit of
              Nothing    -> tellPN "Cannot insert a pixel becasue no visit found for the given visitId"
              Just visit -> do

                let handsetLevel = PV.MidLevel
                config <- ask
                let epixel = PV.pixelUrl gatewayConnection operator (V.campaignId visit) handsetLevel config (V.queryParams visit)

                case epixel of
                  Left e -> tellPN e
                  Right mpixel -> do
                    let (pixelValueId, pixelUrl) = case mpixel of
                            Nothing -> (Nothing, Nothing)
                            Just (pixelValueId, _, pixelUrl)  -> (Just pixelValueId, Just pixelUrl)

                    liftIO $ putStrLn ">> ADDING PIXEL"
                    -- add pixels
                    (pixelId, _ ) <- tryPN $ PX.insertPixel PX.Pixel {
                        PX.creationTime = defaultTime
                      , PX.lastUpdated = Nothing
                      , PX.salesId = salesId
                      , PX.visitId = visitId
                      , PX.pixelState = PX.NotProcessed
                      , PX.pixelUrl = fromStrict <$> pixelUrl
                      , PX.pixelValueId = pixelValueId
                      , PX.pixelResultStatusCode = Nothing
                      , PX.pixelResultText = Nothing
                      } conn

                    tellPN $ "Pixel inserted: " <> T.pack (show pixelId)
  where
    getParams ciParams = do
      let look = lookup' ciParams
      operator <-  look "oid" >>= toOperator
      service  <-  look "service" >>= toService
      return (operator, service)
    lookup' m t = M.lookup t m
    toOperator = lookup' $ M.fromList [("1", O.TR_TUCKCEL), ("4", O.TR_AVEA), ("5", O.TR_VODAFONE)]
    toService  = lookup' $ M.fromList [("2616", GS.AppRing)]
