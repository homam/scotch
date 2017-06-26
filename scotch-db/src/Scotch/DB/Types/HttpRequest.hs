{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric
#-}

{-|
Module      : Scotch.DB.Types.HttpRequest
Description : Representing a PostgreSQL record of a standard HTTP Request

This module is not used anywhere at the moment.
-}
module Scotch.DB.Types.HttpRequest (
    HttpRequest(..)
  , makeHttpRequest
  , parseEncodedParams
  , parseEncodedParamsText
  , queryStringParam
)
where

import Prelude hiding (concat)
import Data.Text.Lazy (Text, fromStrict, concat)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Encoding as Encoding
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as Char8
import qualified Database.PostgreSQL.Simple as PS
import qualified Data.Aeson as A
import GHC.Generics (Generic)
import Scotch.DB.FieldParserHelpers ()
import qualified Web.Scotty.Trans as Trans
import Web.Scotty (parseParam, parseParamList, Parsable)
import Data.Maybe (fromMaybe)
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HTTP.Types
import Control.Arrow ((***))
import qualified Data.Map as M

parseEncodedParams :: Char8.ByteString -> [Trans.Param]
parseEncodedParams bs = [ (fromStrict k, fromStrict $ fromMaybe "" v) | (k,v) <- HTTP.Types.parseQueryText bs ]

parseEncodedParamsText :: Text -> [Trans.Param]
parseEncodedParamsText = parseEncodedParams . BL.toStrict . encodeUtf8

queryStringParam :: Parsable b => Text -> [Trans.Param] -> Either Text b
queryStringParam key list =
    let val = lookup key list
    in  case val of
          Nothing -> Left (concat ["Param Not Found ", key])
          Just v  -> parseParam v

makeHttpRequest :: Wai.Request -> [(Text, Text)] -> [Trans.Param] -> IO HttpRequest
makeHttpRequest req headers allParams =     do
    rawBody <- BL.fromStrict <$> Wai.requestBody req
    let bsToText = fromStrict . Encoding.decodeUtf8
    let rawQueryString = Wai.rawQueryString req
    let queryString = map (bsToText *** fmap bsToText) $ HTTP.Types.parseQuery rawQueryString
    let rawPath = bsToText $ Wai.rawPathInfo req
    let parsedQueryString = HTTP.Types.parseQueryText rawQueryString
    -- let rawHeaders = Wai.requestHeaders req
    return HttpRequest {
        queryString = queryString
      , allParams = allParams
      , headers = headers
      , rawQueryString = bsToText rawQueryString
      , rawPath = rawPath
      -- , rawHeaders
      , rawBody = decodeUtf8 rawBody
    }

data HttpRequest = HttpRequest {
  queryString :: [(Text, Maybe Text)] -- [(Char8.ByteString, Maybe Char8.ByteString)]
, allParams :: [(Text, Text)]
, headers :: [(Text, Text)]
, rawBody :: Text
-- , rawHeaders :: Text
, rawPath :: Text -- Char8.ByteString
, rawQueryString :: Text -- Char8.ByteString
-- , allParamsMap :: M.Map Text Text
} deriving (Show, Generic)

instance PS.ToRow HttpRequest
instance PS.FromRow HttpRequest

instance A.ToJSON HttpRequest
instance A.FromJSON HttpRequest
