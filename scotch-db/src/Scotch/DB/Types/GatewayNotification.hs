{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric
#-}

{-|
Module      : Scotch.DB.Types.GatewayNotification
Description : Represents a PostgreSQL record of a generic HTTP Postback Notification from a Gateway
-}
module Scotch.DB.Types.GatewayNotification (
    GatewayNotification(..)
  , makeGatewayNotification
  , NotificationType(..)
  , GatewayConnection(..)
  , AsyncTaskStatus(..)
)
where

import Prelude hiding (concat)
import Data.Text.Lazy (Text, fromStrict, toLower, concat, pack, replace)
import qualified Data.Text.Encoding as Encoding
import qualified Database.PostgreSQL.Simple as PS
import Database.PostgreSQL.Simple.ToField (toField)
import Database.PostgreSQL.Simple.ToRow (toRow)
import qualified Data.Aeson as A
import GHC.Generics (Generic)
import Scotch.DB.FieldParserHelpers ()
import qualified Web.Scotty.Trans as Trans
import qualified Network.Wai as Wai
import qualified Data.Map as M
import qualified Data.Time as Time
import Scotch.DB.QueryHelpers (defaultTime)
import qualified Web.Scotty as Scotty


makeGatewayNotification :: GatewayConnection -> NotificationType -> Wai.Request -> [Trans.Param] -> GatewayNotification
makeGatewayNotification gatewayConnection notificationType req allParams =
    let bsToText = fromStrict . Encoding.decodeUtf8
        rawQueryString = Wai.rawQueryString req
        rawPath = bsToText $ Wai.rawPathInfo req
    in GatewayNotification {
        gateway_notification_id = 0 -- auto generated value
      , creation_time = defaultTime -- auto generated value
      , allParams = M.fromList allParams
      , rawQueryString = bsToText rawQueryString
      , rawPath = rawPath
      , notificationType = notificationType
      , gatewayConnection = gatewayConnection
      , taskStatus = NotStarted -- default value
      , taskResult = Nothing -- defauklt value
      , taskLastUpdatedTime = Nothing
    }

data NotificationType = SubscriptionNotification | BillingNotification | UnsubscriptionNotification
  deriving (Show, Read, Eq, Generic, Enum, Bounded)
instance A.ToJSON NotificationType
instance A.FromJSON NotificationType

data GatewayConnection = PayGuruStandard
  deriving (Show, Read, Eq, Generic, Enum, Bounded)
instance A.ToJSON GatewayConnection
instance A.FromJSON GatewayConnection

instance Scotty.Parsable GatewayConnection where
    parseParam txt =
        let dic = M.fromList $ map (\g -> (toLower . pack . show $ g, g)) [minBound :: GatewayConnection ..]
        in case M.lookup (toLower txt) dic of
          Just gw -> Right gw
          Nothing -> Left $ concat ["Unable to parse ", txt, " to a GatewayConnection"]

instance Scotty.Parsable NotificationType where
    parseParam txt =
      let dic = M.fromList $ map (\g -> (replace "notification" "" . toLower . pack . show $ g, g)) [minBound ..]
      in case M.lookup (toLower txt) dic of
        Just gw -> Right gw
        Nothing -> Left $ concat ["Unable to parse ", txt, " to a NotificationType"]


data AsyncTaskStatus = NotStarted | Started | Completed | Failed
  deriving (Show, Read, Eq, Generic, Enum)
instance A.ToJSON AsyncTaskStatus
instance A.FromJSON AsyncTaskStatus

-- |
-- Example:
--
-- > get "/notification" $ do
-- >   req <- request
-- >   allParams <- params
-- >   let notification = makeGatewayNotification
-- >         PayGuruStandard
-- >         SubscriptionNotification
-- >         req
-- >         allParams
-- >   res <- tryQuery (addGatewayNotification notification)
-- >   (text . pack . show ||| json) res
data GatewayNotification = GatewayNotification {
  gateway_notification_id :: Int
, creation_time :: Time.ZonedTime
, allParams :: M.Map Text Text
, rawPath :: Text
, rawQueryString :: Text
, notificationType :: NotificationType
, gatewayConnection :: GatewayConnection
, taskStatus :: AsyncTaskStatus
, taskResult :: Maybe Text
, taskLastUpdatedTime :: Maybe Time.ZonedTime
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
      , toField (taskStatus d)
      , toField (taskResult d)
      , toField (taskLastUpdatedTime d)
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
