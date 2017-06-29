{-# LANGUAGE
    OverloadedStrings
#-}

module Scotch.DB.IsGateway (
  IsGateway(..)
)
where

import Scotch.DB.Types
import Data.Text.Lazy (Text)
import Web.Scotty.Trans
import Scotch.DB.Types.GatewayNotification as GatewayNotification

class IsGateway g where
    getFlow' :: (Monad a) => g -> LandingPage -> ActionT Text a ()
    processNotification :: g -> GatewayNotification.GatewayNotification -> IO ()
    identifier :: g -> GatewayNotification.GatewayConnection