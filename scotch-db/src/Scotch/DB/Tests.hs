{-# LANGUAGE
    OverloadedStrings
  , MultiParamTypeClasses
  , GeneralizedNewtypeDeriving
  , NamedFieldPuns
  , FlexibleContexts
  , RankNTypes
  , TupleSections
#-}

module Scotch.DB.Tests(
    someFunc
  , Operator (..)
  , AffiliateId (..)
  , Affiliate (..)
  , standardAffiliate
  , runPixelValueGetter
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
import Scotch.DB.Types
-- import GHC.Generics (Generic)
import Data.Decimal
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
          ((PayguruTurkey, Nothing, Nothing, Just HighEnd), Just (0.8 , NoPixelValue))
        , ((PayguruTurkey, Just TR_AVEA, Just (AffiliateId "BillyMob"), Nothing), Just (0.5 , NoPixelValue))
        ]
  -- create an instance of readonly configuration using values retrieved from the databae
  let config = PixelValueConfig { campaignIdMap, pixelMap }

  -- get the pixel value for PayguruTurkey and TR_AVEA and CampaignId = 2 and HighEnd phones
  let pixelValueGetter = pixelValue PayguruTurkey TR_AVEA (CampaignId 2) HighEnd

  -- get pixel value result
  case runPixelValueGetter pixelValueGetter config of
    Left e -> putStrLn $ T.unpack e
    Right mpv -> case mpv of
      Nothing -> putStrLn "No Pixel should be fired"
      Just (affiliate, (probability, pixelValue)) -> do
        putStrLn $ "Fire pixel " ++ show pixelValue ++ " with probability of " ++ show probability
        let visitQueryString = M.fromList [("click_id", "28311")] --- get this from the database
        putStrLn $ T.unpack $ getPixelUrl affiliate visitQueryString pixelValue

-- | Utility function for formatting texts by mustache syntax: "{parameter}"
formatText :: M.Map T.Text T.Text -> T.Text -> T.Text
formatText m t = foldl
  (\x (k, v) -> T.replace (T.concat ["{", k, "}"]) v x)
  t
  (M.toList m)


-- | Enum of operators
data Operator = TR_AVEA | TR_TUCKCEL | TR_VODAFONE
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- | Represents different ways of representing the value of the pixel
data PixelValue =
    DecimalPixelValue Decimal -- ^ Value of pixel is represneted by a decimal number in the URL
  | PerOperatorPixel Operator -- ^ Value is represented by the name of the operator on the URL
  | ParametricPixelValue (M.Map T.Text T.Text) -- ^ Customizable
  | NoPixelValue -- ^ Pixel value is not represented in the URL
  deriving (Show, Eq)

-- | Converts a 'PixelValue' to a Map that can be used for constructing a URL
pixelValueToMap :: PixelValue -> M.Map T.Text T.Text
pixelValueToMap (DecimalPixelValue d) = M.fromList [("pixle_value", T.pack $ show d)]
pixelValueToMap (PerOperatorPixel o) = M.fromList [("operator", T.pack $ show o)]
pixelValueToMap (ParametricPixelValue m) = m
pixelValueToMap NoPixelValue = M.empty

-- | Various types of handset levels that affect the value of pixels
data HandsetLevel = HighEnd | MidLevel | LowEnd
  deriving (Show, Read, Eq, Ord)

-- | Affiliate Id
newtype AffiliateId = AffiliateId String
  deriving (Show, Read, Eq, Ord)

-- | Affiliates are instances of this type
data Affiliate = Affiliate {
    getPixelUrl :: M.Map T.Text T.Text -> PixelValue -> T.Text
  , affiliateId :: AffiliateId
  }
instance Show Affiliate where
  show = show . affiliateId

-- | Constructs a standard affiliate
-- Visit query string of the standard affiliate: ?clickid=123456&publisher=abcdef
-- URL template: http://www.affiliate.com/tack?click_id={clickid}&pixel={pixel_value}
-- > getPixelUrl standard (M.fromList [("clickid", "123456"), ("publisher", "abcdef")]) (DecimalPixelValue 1.8)
-- > -- Constructed Pixel URL: http://www.affiliate.com/tack?click_id=123456&pixel=1.8
standardAffiliate :: AffiliateId -> T.Text -> Affiliate
standardAffiliate affId turl = Affiliate {
    getPixelUrl = \ visitQueryString pixelValue -> formatText (M.union visitQueryString (pixelValueToMap pixelValue)) turl
  , affiliateId = affId
  }

pixelValue :: GatewayConnection -> Operator -> CampaignId -> HandsetLevel -> PixelValueGetter (Maybe (Affiliate, (Double, PixelValue)))
pixelValue g o cid h = do
  c <- ask
  case M.lookup cid (campaignIdMap c) of
    Nothing -> PixelValueGetter $ throwE $ T.concat ["No affiliate was found for CampaignId = ", T.pack $ show cid]
    Just aff -> return $ (aff, ) <$> runReader (go g o (affiliateId aff) h) (pixelMap c)
    where
      -- hardcoded rules
      go PayguruTurkey TR_AVEA _ HighEnd = return $ Just (1, DecimalPixelValue $ Decimal 2 110)
      go PayguruTurkey TR_AVEA (AffiliateId "BillyMob") _ = return $ Just (1, DecimalPixelValue $ Decimal 2 110)
      go PayguruTurkey TR_AVEA _ _ = return $ Just (1, DecimalPixelValue $ Decimal 2 150)

      -- generic rule
      go g o a h = do
        m <- ask
        return $ find m [
            (g, Just o, Just a, Just h)
          , (g, Just o, Just a, Nothing)
          , (g, Just o, Nothing, Just h)
          , (g, Nothing, Nothing, Nothing)
          ]

      find _ [] = Just (1, NoPixelValue)
      find m (x:xs) = fromMaybe (find m xs) (M.lookup x m)

-- | Read-only configuration that is used in `pixelValue` function.
data PixelValueConfig = PixelValueConfig {
    campaignIdMap :: M.Map CampaignId Affiliate
  , pixelMap :: M.Map (GatewayConnection, Maybe Operator, Maybe AffiliateId, Maybe HandsetLevel) (Maybe (Double, PixelValue))
  }

newtype PixelValueGetter a = PixelValueGetter {
    unPixelValueGetter :: ExceptT T.Text (Reader PixelValueConfig) a
  } deriving (Functor, Applicative, Monad, MonadReader PixelValueConfig, MonadError T.Text)

runPixelValueGetter = runReader . runExceptT . unPixelValueGetter
