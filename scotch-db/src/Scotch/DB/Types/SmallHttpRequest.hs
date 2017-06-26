{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric
#-}

module Scotch.DB.Types.SmallHttpRequest (
    SmallHttpRequest(..)
  , makeSmallHttpRequest
)
where

import Prelude hiding (concat)
import Data.Text.Lazy (Text, fromStrict)
import qualified Data.Text.Encoding as Encoding
import qualified Database.PostgreSQL.Simple as PS
import qualified Data.Aeson as A
import GHC.Generics (Generic)
import Scotch.DB.FieldParserHelpers ()
import qualified Web.Scotty.Trans as Trans
import qualified Network.Wai as Wai
import qualified Data.Map as M

makeSmallHttpRequest :: Wai.Request -> [Trans.Param] -> SmallHttpRequest
makeSmallHttpRequest req allParams =
    let bsToText = fromStrict . Encoding.decodeUtf8
        rawQueryString = Wai.rawQueryString req
        rawPath = bsToText $ Wai.rawPathInfo req
    in SmallHttpRequest {
        allParams = M.fromList allParams
      , rawQueryString = bsToText rawQueryString
      , rawPath = rawPath
    }

-- |
-- Example:
--
-- > post "/:a" $ do
-- >   req <- request
-- >   allParams <- params
-- >   let requestModel = makeSmallHttpRequest req allParams
data SmallHttpRequest = SmallHttpRequest {
  allParams :: M.Map Text Text
, rawPath :: Text
, rawQueryString :: Text
} deriving (Show, Generic)

instance PS.ToRow SmallHttpRequest
instance PS.FromRow SmallHttpRequest

instance A.ToJSON SmallHttpRequest
instance A.FromJSON SmallHttpRequest
