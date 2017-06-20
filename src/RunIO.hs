{-# LANGUAGE
    GeneralizedNewtypeDeriving
#-}

module RunIO (
  RunIO, runIO, liftRunIO, throwRunIO
) where

import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Except as ME
import Control.Monad.Trans (liftIO)


type SomeError = String

newtype RunIO a = RunIO {
  unIO :: E.ExceptT SomeError IO a
} deriving (Functor, Applicative, Monad, ME.MonadError SomeError, ME.MonadIO)


runIO :: RunIO a -> IO (Either SomeError a)
runIO = ME.runExceptT . unIO

liftRunIO :: IO a -> RunIO a
liftRunIO = liftIO

throwRunIO :: SomeError -> RunIO a
throwRunIO = RunIO . E.throwE
