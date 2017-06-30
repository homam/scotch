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
    handleVisit _ (LandingPage lp) = text lp >> return RedirectToPaymentPage
    processNotification _ _ = undefined
    identifier _ = GatewayNotification.TestStandard
