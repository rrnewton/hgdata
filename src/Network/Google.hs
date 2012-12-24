-----------------------------------------------------------------------------
--
-- Module      :  Network.Google
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------


module Network.Google (
  AccessToken
, makeRequest
, toAccessToken
) where


import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as S8
import qualified Data.CaseInsensitive as CI
import Network.HTTP.Conduit (Request(..), def)


type AccessToken = BS.ByteString


toAccessToken :: String -> AccessToken
toAccessToken = S8.pack


makeRequest :: String -> AccessToken -> Request m
makeRequest path accessToken =
  def {
    method = S8.pack "GET"
  , secure = True
  , host = S8.pack "www.google.com"
  , port = 443
  , path = S8.pack path
  , queryString = S8.pack "?max-results=100000"
  , requestHeaders = [
      (mkHeaderName "Gdata-version", S8.pack "3.0")
    , (mkHeaderName "Authorization",  S8.append (S8.pack "OAuth ") accessToken)
    ]
  }
    where
      mkHeaderName = CI.mk . S8.pack
