{-# LANGUAGE
    OverloadedStrings
  , RankNTypes
  , GeneralizedNewtypeDeriving
  , ScopedTypeVariables
  , MultiParamTypeClasses
  , TypeFamilies
  , UndecidableInstances
  , FlexibleInstances
#-}

module Scotch.DB.IsGateway (
    IsGateway(..)
  , ProcessNotificationResult
  , throwPN, tellPN, tryPN, ask
  , lift, liftIO, runProcessNotification
)
where

import Scotch.DB.Types.Imports
import Scotch.DB.Types
import Scotch.DB.Types.PixelValue (PixelValueUrlRepresentationConfig (..))
import qualified Data.Text as T
import Web.Scotty.Trans
import Scotch.DB.Types.GatewayNotification as GatewayNotification
import Control.Monad.Except (MonadError (..))
import Control.Monad.Trans.Except
import Control.Monad.Writer.Strict (MonadWriter (..), lift, MonadIO, liftIO)
import Control.Monad.Trans.Writer.Strict (WriterT, runWriterT)
import Control.Monad.Trans.Reader hiding (ask)
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.Trans.Control
import Control.Monad.Base
                 -- Control.Monad.Trans.Control.MonadBaseControl
import Control.Exception (try, SomeException)

newtype Foo a = Foo { unFoo :: ReaderT Int IO a }
  deriving (Monad, Applicative, Functor, MonadBase IO)

instance MonadBaseControl IO Foo where
  type StM Foo a = a
  liftBaseWith f = Foo $ liftBaseWith $ \q -> f (q . unFoo)
  restoreM = Foo . restoreM

type InnerPR = ReaderT PixelValueUrlRepresentationConfig (WriterT [T.Text] (ExceptT T.Text IO))

newtype ProcessNotificationResult a = ProcessNotificationResult {
    unProcessNotification :: InnerPR a
  } deriving (Functor, Applicative, Monad, MonadWriter [T.Text], MonadError T.Text, MonadIO, MonadReader PixelValueUrlRepresentationConfig)

instance MonadBase IO ProcessNotificationResult where liftBase = tryPN

-- instance MonadTransControl ProcessNotificationResult where
--   type StT ProcessNotificationResult m = StT (InnerPR m) m
--   liftWith = defaultLiftWith ProcessNotificationResult unProcessNotification
--   restoreT = defaultRestoreT ProcessNotificationResult

instance MonadBaseControl IO ProcessNotificationResult where
  type StM ProcessNotificationResult a = Either T.Text (a, [T.Text])
  liftBaseWith f = ProcessNotificationResult $ liftBaseWith $ \q -> f (q . unProcessNotification)
  -- liftBaseWith f = ProcessNotificationResult $ liftBaseWith $ \q -> f ( \m -> _ . runExceptT . runWriterT . runReaderT (unProcessNotification m)) -- StM -- f = tryPN --  f id;
  restoreM = ProcessNotificationResult . restoreM -- return;

runProcessNotification :: ProcessNotificationResult () -> PixelValueUrlRepresentationConfig -> IO (Either T.Text ((), [T.Text]))
runProcessNotification res = runExceptT . runWriterT . runReaderT (unProcessNotification res)

throwPN :: T.Text -> ProcessNotificationResult a
throwPN = ProcessNotificationResult . lift . lift . throwE

tellPN :: T.Text -> ProcessNotificationResult ()
tellPN = ProcessNotificationResult . tell . (:[])

tryPN ::  IO a -> ProcessNotificationResult a
tryPN m = do
  (x :: Either SomeException a) <- liftIO $ try m
  case x of
    Left e -> throwPN (T.pack $ show e)
    Right y -> return y

class IsGateway g where
  handleVisit :: (Monad a) => g -> LandingPage -> ActionT Text a OptInMethod
  processNotification :: g -> GatewayNotification.GatewayNotification -> Connection -> ProcessNotificationResult () -- ExceptT Text IO ()
  identifier :: g -> GatewayNotification.GatewayConnection
