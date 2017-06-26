{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric
#-}

module Scotch.DB.Types.GatewayNotification (
    GatewayNotification(..)
  , makeGatewayNotification
  , NotificationType(..)
  , GatewayConnection(..)
)
where

import Prelude hiding (concat)
import Data.Text.Lazy (Text, fromStrict)
import qualified Data.Text.Encoding as Encoding
import qualified Database.PostgreSQL.Simple as PS
import Database.PostgreSQL.Simple.ToField (toField, ToField)
import Database.PostgreSQL.Simple.FromField (fromField, FromField)
import Database.PostgreSQL.Simple.ToRow (toRow)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Aeson as A
import GHC.Generics (Generic)
import Scotch.DB.FieldParserHelpers ()
import qualified Web.Scotty.Trans as Trans
import qualified Network.Wai as Wai
import qualified Data.Map as M
import qualified Data.Time as Time
import Scotch.DB.QueryHelpers (defaultTime)


makeGatewayNotification :: GatewayConnection -> NotificationType -> Wai.Request -> [Trans.Param] -> GatewayNotification
makeGatewayNotification gatewayConnection notificationType req allParams =
    let bsToText = fromStrict . Encoding.decodeUtf8
        rawQueryString = Wai.rawQueryString req
        rawPath = bsToText $ Wai.rawPathInfo req
    in GatewayNotification {
        gateway_notification_id = 0
      , creation_time = defaultTime
      , allParams = M.fromList allParams
      , rawQueryString = bsToText rawQueryString
      , rawPath = rawPath
      , notificationType = notificationType
      , gatewayConnection = gatewayConnection
    }

data NotificationType = SubscriptionNotification | BillingNotification | UnsubscriptionNotification
    deriving (Show, Read, Eq, Generic, Enum)

instance ToField NotificationType where
    toField = toField . show
instance FromField NotificationType where
    fromField _ Nothing = error "Cannot parse Nothing"
    fromField _ (Just bs) = return $ read $ Char8.unpack $ BL.toStrict $ BL.fromStrict bs


instance A.ToJSON NotificationType
instance A.FromJSON NotificationType


data GatewayConnection = PayGuruStandard
    deriving (Show, Read, Eq, Generic, Enum)

instance ToField GatewayConnection where
    toField = toField . show
instance FromField GatewayConnection where
    fromField _ Nothing = error "Cannot parse Nothing"
    fromField _ (Just bs) = return $ read $ Char8.unpack $ BL.toStrict $ BL.fromStrict bs


instance A.ToJSON GatewayConnection
instance A.FromJSON GatewayConnection


-- |
-- Example:
--
-- > post "/:a" $ do
-- >   req <- request
-- >   allParams <- params
-- >   let requestModel = makeGatewayNotification req allParams
data GatewayNotification = GatewayNotification {
  gateway_notification_id :: Int
, creation_time :: Time.ZonedTime
, allParams :: M.Map Text Text
, rawPath :: Text
, rawQueryString :: Text
, notificationType :: NotificationType
, gatewayConnection :: GatewayConnection
} deriving (Show, Generic)

instance PS.ToRow GatewayNotification where
  toRow d = [
    --   toField (gateway_notification_id d)
    --   toField (creation_time d)
        toField (allParams d)
      , toField (rawPath d)
      , toField (rawQueryString d)
      , toField (notificationType d)
      , toField (gatewayConnection d)
    ]
instance PS.FromRow GatewayNotification

instance A.ToJSON GatewayNotification
instance A.FromJSON GatewayNotification

---

-- process :: GatewayNotification -> IO ()
-- process notification = case gatewayConnection notification of
--     PayGuruStandard -> processPayGuruNotification notification
--
-- processPayGuruNotification :: GatewayNotification -> IO ()
-- processPayGuruNotification n = case notificationType n of
--     SubscriptionNotification -> return ()
--     BillingNotification -> return ()
--     _ -> return ()
