{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
#-}

module TestPixelValues (
  main
) where

import qualified System.Environment as Env
import qualified Data.Pool as P
import qualified Data.Time as Time
import qualified Data.ByteString.Char8 as Char8
import qualified Scotch.DB.Types.PixelValue as PV
import Scotch.DB.QueryHelpers (myPool)
import Scotch.DB.Types.Operator (Operator (..))
import Scotch.DB.Types.GatewayConnection
import Scotch.DB.Types.Affiliate (Affiliate, AffiliateId (..), PixelValueUrlRepresentation (..), HandsetLevel (..))
import qualified Scotch.DB.Types.Affiliate as AF
import Scotch.DB.Types (CampaignId)

db_addNewPixelValueDB pool = do
  (newPixelID :: Int, newCreationTime :: Time.ZonedTime) <- P.withResource pool $
    PV.addNewPixelValueDB
      "Homam"
      PayguruTurkey
      (Just TR_TUCKCEL) -- operator
      Nothing -- AffiliateId
      Nothing -- HandsetLevel
      Nothing -- hardcodedValueDescription
      1.0 -- pixelFiringRatio
      NoPixelValueUrlRepresentation
      (Just 1.1) -- pixelMonetaryValue
  print (newPixelID, newCreationTime)

db_addNewAffiliate pool = do
  res <- P.withResource pool $
    AF.addNewAffiliateDB
      "HttpBin"
      "Homam"
      "https://httpbin.org/get?click_id={click_id}&payout={pixel_value}"
  print res


main = do
  putStrLn "starting..."
  connectionString <- Env.getEnv "SCOTCH_DB"
  pool <- myPool (Char8.pack connectionString)
  db_addNewAffiliate pool
