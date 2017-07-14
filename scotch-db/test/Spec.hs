{-# LANGUAGE OverloadedStrings, CPP #-}

-- import qualified Test.Hspec as H
import qualified TestPixelValues
import qualified TestVisit

import           Test.Hspec
import           Test.Hspec.Wai
import           Network.Wai (Application)

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.String
import           Network.HTTP.Types
import           Web.Scotty as Scotty hiding (get, post, put, patch, delete, request, options)
import qualified Web.Scotty as Scotty

main :: IO ()
main = TestVisit.main

-- main :: IO ()
-- main = hspec spec

withApp :: ScottyM () -> SpecWith Application -> Spec
withApp = with . scottyApp

spec :: Spec
spec = do
  describe "ScottyM" $ do
    forM_ [
        ("GET", Scotty.get, get)
      , ("POST", Scotty.post, (`post` ""))
      , ("PUT", Scotty.put, (`put` ""))
      , ("PATCH", Scotty.patch, (`patch` ""))
      , ("DELETE", Scotty.delete, delete)
      , ("OPTIONS", Scotty.options, options)
      ] $ \(method, route, makeRequest) -> do
      describe (map toLower method) $ do
        withApp (route "/scotty" $ html "") $ do
          it ("adds route for " ++ method ++ " requests") $ do
            makeRequest "/scotty" `shouldRespondWith` 200
