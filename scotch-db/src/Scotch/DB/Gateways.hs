module Scotch.DB.Gateways (
    AllGateways(..)
  , module Scotch.DB.IsGateway
  , module Scotch.DB.Gateways.PayGuruGateway
  , module Scotch.DB.Gateways.TestGateway
)
where

import Scotch.DB.IsGateway
import Scotch.DB.Gateways.PayGuruGateway
import Scotch.DB.Gateways.TestGateway

data AllGateways =
        PayGuru PayGuruGateway
    |   Test TestGateway

instance IsGateway AllGateways where
    getFlow' (PayGuru g) = getFlow' g
    getFlow' (Test g) = getFlow' g

    processNotification (PayGuru g) = processNotification g
    processNotification (Test g) = processNotification g

    identifier (PayGuru g) = identifier g
    identifier (Test g) = identifier g
