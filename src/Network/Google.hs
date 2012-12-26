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


{-# LANGUAGE FlexibleInstances #-}


module Network.Google (
  AccessToken
, appendBody
, appendHeaders
, appendQuery
, doManagedRequest
, doRequest
, makeProjectRequest
, makeRequest
, makeRequestValue
, toAccessToken
) where


import Control.Monad.Trans.Resource -- (MonadResource, ReleaseKey, ResourceT, allocate, runResourceT)
import Data.List (intersperse)
import Data.Maybe (fromJust)
import Data.ByteString.Util (lbsToS)
import Data.ByteString as BS (ByteString)
import Data.ByteString.Char8 as BS8 (ByteString, append, pack, unpack)
import Data.ByteString.Lazy.Char8 as LBS8 (ByteString)
import Data.CaseInsensitive as CI (CI(..), mk)
import Network.HTTP.Base (urlEncode)
import Network.HTTP.Conduit (Manager, Request(..), RequestBody(..), Response(..), closeManager, def, httpLbs, newManager, responseBody, withManager)
import Text.XML.Light (Element, parseXMLDoc)


type AccessToken = BS.ByteString


toAccessToken :: String -> AccessToken
toAccessToken = BS8.pack


makeRequest :: AccessToken -> (String, String) -> String -> (String, String) -> Request m
makeRequest accessToken (apiName, apiVersion) method (host, path) =
  def {
    method = BS8.pack method
  , secure = True
  , host = BS8.pack host
  , port = 443
  , path = BS8.pack path
  , requestHeaders = [
      (makeHeaderName apiName, BS8.pack apiVersion)
    , (makeHeaderName "Authorization",  BS8.append (BS8.pack "OAuth ") accessToken)
    ]
  }


makeProjectRequest :: String -> AccessToken -> (String, String) -> String -> (String, String) -> Request m
makeProjectRequest projectId accessToken api method hostPath =
  appendHeaders
    [
      ("x-goog-project-id", projectId)
    ]
    (makeRequest accessToken api method hostPath)


class DoRequest a where
  doRequest :: Request (ResourceT IO) -> IO a
  doRequest request =
    do
--    (_, manager) <- allocate (newManager def) closeManager
      manager <- newManager def
      result <- doManagedRequest manager request
      closeManager manager
      return result
  doManagedRequest :: Manager -> Request (ResourceT IO) -> IO a


instance DoRequest LBS8.ByteString where
  doManagedRequest manager request =
    do
      response <- runResourceT (httpLbs request manager)
      return $ responseBody response


instance DoRequest String where
  doManagedRequest manager request =
    do
      result <- doManagedRequest manager request
      return $ lbsToS result


instance DoRequest [(String, String)] where
  doManagedRequest manager request =
    do
      response <- runResourceT (httpLbs request manager)
      return $ read . show $ responseHeaders response


instance DoRequest () where
  doManagedRequest manager request =
    do
      doManagedRequest manager request :: IO LBS8.ByteString
      return ()


instance DoRequest Element where
  doManagedRequest manager request =
    do
      result <- (doManagedRequest manager request :: IO LBS8.ByteString)
      return $ fromJust $ parseXMLDoc result


makeRequestValue :: String -> BS8.ByteString
makeRequestValue = BS8.pack


makeHeaderName :: String -> CI.CI BS8.ByteString
makeHeaderName = CI.mk . BS8.pack


makeHeaderValue :: String -> BS8.ByteString
makeHeaderValue = BS8.pack


appendHeaders :: [(String, String)] -> Request m -> Request m
appendHeaders headers request =
  let
    headerize :: (String, String) -> (CI.CI BS8.ByteString, BS8.ByteString)
    headerize (n, v) = (makeHeaderName n, makeHeaderValue v)
  in
    request {
      requestHeaders = requestHeaders request ++ map headerize headers
    }


appendBody :: LBS8.ByteString -> Request m -> Request m
appendBody bytes request =
  request {
    requestBody = RequestBodyLBS bytes
  }


appendQuery :: [(String, String)] -> Request m -> Request m
appendQuery query request =
  let
    makeParameter :: (String, String) -> String
    makeParameter (k, v) = k ++ "=" ++ urlEncode v
    query' :: String
    query' = concat $ intersperse "&" $ map makeParameter query
  in
    request
      {
        queryString = BS8.pack $ "?" ++ query'
      }

