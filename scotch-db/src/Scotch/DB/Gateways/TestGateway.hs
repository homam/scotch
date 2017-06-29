{-# LANGUAGE
    OverloadedStrings
#-}

module Scotch.DB.Gateways.TestGateway (
  TestGateway (..)
)
where

import Scotch.DB.IsGateway
import Scotch.DB.Types
-- import Data.Text.Lazy (Text, pack)
import Web.Scotty.Trans
import Scotch.DB.Types.GatewayNotification as GatewayNotification

data TestGateway = TestGateway

instance IsGateway TestGateway where
    getFlow' _ (LandingPage lp) = text lp
    processNotification _ _ = undefined
    identifier _ = GatewayNotification.TestStandard
