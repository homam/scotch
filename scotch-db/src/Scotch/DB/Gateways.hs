module Scotch.DB.Gateways (
    AllGateways(..)
  , getGateway
  , module Scotch.DB.IsGateway
  , module Scotch.DB.Gateways.PayGuruGateway
  , module Scotch.DB.Gateways.TestGateway
)
where

import Scotch.DB.IsGateway
import Scotch.DB.Types.GatewayConnection
import Scotch.DB.Gateways.PayGuruGateway
import Scotch.DB.Gateways.TestGateway

data AllGateways =
    PayGuru PayGuruGateway
  | Test TestGateway

instance IsGateway AllGateways where
  handleVisit (PayGuru g) = handleVisit g
  handleVisit (Test g) = handleVisit g

  processNotification (PayGuru g) = processNotification g
  processNotification (Test g) = processNotification g

  identifier (PayGuru g) = identifier g
  identifier (Test g) = identifier g

getGateway :: GatewayConnection -> AllGateways
getGateway PayguruTurkey = PayGuru PayGuruGateway { payGuruEndPointUrl = "https://httpbin.org/get", payGuruUsername = "pguname" }
getGateway TestStandard = Test TestGateway
