{-# LANGUAGE
    OverloadedStrings
  , MultiParamTypeClasses
  , GeneralizedNewtypeDeriving
  , NamedFieldPuns
  , FlexibleContexts
  , RankNTypes
  , TupleSections
  , DeriveGeneric
#-}

module Scotch.DB.Tests(
    someFunc
  , Operator (..)
  , AffiliateId (..)
  , Affiliate (..)
  , PixelValueUrlRepresentation (..)
  , standardAffiliate
  , runPixelValueUrlRepresentationGetter
)
where
import Data.Maybe (fromMaybe)
import qualified System.Environment as Env
import qualified Data.ByteString.Char8 as Char8
-- import Control.Monad.Trans (liftIO)
-- import Control.Monad (forever)
import qualified Data.Pool as P
-- import qualified Control.Concurrent as C
-- import qualified Scotch.DB.Queries as Q
import qualified Scotch.DB.Types.Pixel as Pixel
import qualified Data.Map as M
import Scotch.DB.QueryHelpers (myPool)
import Scotch.DB.Types.GatewayConnection
import Scotch.DB.Types.Operator (Operator(..))
import Scotch.DB.Types.HandsetLevel (HandsetLevel(..))
import Scotch.DB.Types.Affiliate
import Scotch.DB.Types
import qualified Data.Text as T
import Control.Monad.Reader (MonadReader, ask, Reader, runReader)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import Control.Monad.Except (MonadError)

someFunc :: IO ()
someFunc = do
  -- get `campaignIdMap` from the database
  -- it maps 'campaignId's to 'AffiliateId's
  let campaignIdMap = M.fromList [
        (CampaignId 2, standardAffiliate (AffiliateId "BillyMob") "http://pixel.billymob.com/?clickid={click_id}&v={pixle_value}")
        ]
  -- get 'pixelMap' from a database.
  -- It maps certain parameters to a pixel value
  let  pixelMap = M.fromList [
          ((PayguruTurkey, Nothing, Nothing, Just HighEnd), Just (0.8 , NoPixelValueUrlRepresentation))
        , ((PayguruTurkey, Just TR_AVEA, Just (AffiliateId "BillyMob"), Just HighEnd), Just (0.5 , NoPixelValueUrlRepresentation))
        ]
  -- create an instance of readonly configuration using values retrieved from the databae
  let config = PixelValueUrlRepresentationConfig { campaignIdMap, pixelMap }

  -- get the pixel value for PayguruTurkey and TR_AVEA and CampaignId = 2 and HighEnd phones
  let pixelValueGetter = pixelValue PayguruTurkey TR_AVEA (CampaignId 2) HighEnd

  -- get pixel value result
  case runPixelValueUrlRepresentationGetter pixelValueGetter config of
    Left e -> putStrLn $ T.unpack e
    Right mpv -> case mpv of
      Nothing -> putStrLn "No Pixel should be fired"
      Just (affiliate, (probability, pixelValue)) -> do
        putStrLn $ "Fire pixel i" ++ show pixelValue ++ " with probability of " ++ show probability
        let visitQueryString = M.fromList [("click_id", "28311")] --- get this from the database
        putStrLn $ T.unpack $ getPixelUrl affiliate visitQueryString pixelValue



pixelValue :: GatewayConnection -> Operator -> CampaignId -> HandsetLevel -> PixelValueUrlRepresentationGetter (Maybe (Affiliate, (Double, PixelValueUrlRepresentation)))
pixelValue g o cid h = do
  c <- ask
  case M.lookup cid (campaignIdMap c) of
    Nothing -> PixelValueUrlRepresentationGetter $ throwE $ T.concat ["No affiliate was found for CampaignId = ", T.pack $ show cid]
    Just aff -> return $ (aff, ) <$> runReader (go g o (affiliateId aff) h) (pixelMap c)
    where
      -- hardcoded rules
      -- go PayguruTurkey TR_AVEA _ HighEnd = return $ Just (1, DecimalPixelValueUrlRepresentation $ Decimal 2 110)
      -- go PayguruTurkey TR_AVEA (AffiliateId "BillyMob") _ = return $ Just (1, DecimalPixelValueUrlRepresentation $ Decimal 2 110)
      -- go PayguruTurkey TR_AVEA _ _ = return $ Just (1, DecimalPixelValueUrlRepresentation $ Decimal 2 150)

      -- generic rule
      go g o a h = do
        m <- ask
        return $ find m [
            (g, Just o, Just a, Just h)
          , (g, Just o, Just a, Nothing)
          , (g, Just o, Nothing, Just h)
          , (g, Nothing, Nothing, Nothing)
          ]

      find _ [] = Just (1, NoPixelValueUrlRepresentation)
      find m (x:xs) = fromMaybe (find m xs) (M.lookup x m)

-- | Read-only configuration that is used in `pixelValue` function.
data PixelValueUrlRepresentationConfig = PixelValueUrlRepresentationConfig {
    campaignIdMap :: M.Map CampaignId Affiliate
  , pixelMap :: M.Map (GatewayConnection, Maybe Operator, Maybe AffiliateId, Maybe HandsetLevel) (Maybe (Double, PixelValueUrlRepresentation))
  }

newtype PixelValueUrlRepresentationGetter a = PixelValueUrlRepresentationGetter {
    unPixelValueUrlRepresentationGetter :: ExceptT T.Text (Reader PixelValueUrlRepresentationConfig) a
  } deriving (Functor, Applicative, Monad, MonadReader PixelValueUrlRepresentationConfig, MonadError T.Text)

runPixelValueUrlRepresentationGetter = runReader . runExceptT . unPixelValueUrlRepresentationGetter
