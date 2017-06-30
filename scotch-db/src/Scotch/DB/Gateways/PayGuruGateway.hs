{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric
#-}

module Scotch.DB.Gateways.PayGuruGateway (
  PayGuruGateway(..)
)
where

import Scotch.DB.IsGateway
import Scotch.DB.Types
-- import Data.Text.Lazy (Text, pack)
import Web.Scotty.Trans
import Scotch.DB.Types.GatewayNotification as GatewayNotification

data PayGuruGateway = PayGuruGateway {
        payGuruEndPointUrl :: String
      , payGuruUsername :: String
    }

instance IsGateway PayGuruGateway where
    handleVisit _ _ = text "pay flow" >> return RedirectToPaymentPage
    processNotification pg _ = print (payGuruUsername pg)
    identifier _ = GatewayNotification.PayguruTurkey
