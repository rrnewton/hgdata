-----------------------------------------------------------------------------
--
-- Module      :  Network.Google
-- Copyright   :  (c) 2012-13 Brian W Bush
-- License     :  MIT
--
-- Maintainer  :  Brian W Bush <b.w.bush@acm.org>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Helper functions for accessing Google APIs.
--
-----------------------------------------------------------------------------


{-# LANGUAGE FlexibleInstances #-}


module Network.Google (
-- * Types
  AccessToken
, toAccessToken
, ProjectId
-- * Functions
, appendBody
, appendHeaders
, appendQuery
, doManagedRequest
, doRequest
, makeHeaderName
, makeProjectRequest
, makeRequest
, makeRequestValue
) where


import Control.Exception (finally)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Data.ByteString as BS (ByteString)
import Data.ByteString.Char8 as BS8 (ByteString, append, pack)
import Data.ByteString.Lazy.Char8 as LBS8 (ByteString)
import Data.ByteString.Lazy.UTF8 (toString)
import Data.CaseInsensitive as CI (CI(..), mk)
import Network.HTTP.Base (urlEncode)
import Network.HTTP.Conduit (Manager, Request(..), RequestBody(..), Response(..), closeManager, def, httpLbs, newManager, responseBody)
import Text.JSON (JSValue, Result(Ok), decode)
import Text.XML.Light (Element, parseXMLDoc)


-- | OAuth 2.0 access token.
type AccessToken = BS.ByteString

-- | Convert a string to an access token.
toAccessToken ::
     String       -- ^ The string.
  -> AccessToken  -- ^ The OAuth 2.0 access token.
toAccessToken = BS8.pack


-- | Google API project ID, see <https://code.google.com/apis/console>.
type ProjectId = String


-- | Construct a Google API request.
makeRequest ::
     AccessToken       -- ^ The OAuth 2.0 access token.
  -> (String, String)  -- ^ The Google API name and version.
  -> String            -- ^ The HTTP method.
  -> (String, String)  -- ^ The host and path for the request.
  -> Request m         -- ^ The HTTP request.
makeRequest accessToken (apiName, apiVersion) method (host, path) =
  -- TODO: In principle, we should UTF-8 encode the bytestrings packed below.
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


-- | Construct a project-related Google API request.
makeProjectRequest ::
     ProjectId         -- ^ The project ID.
  -> AccessToken       -- ^ The OAuth 2.0 access token.
  -> (String, String)  -- ^ The Google API name and version.
  -> String            -- ^ The HTTP method.
  -> (String, String)  -- ^ The host and path for the request.
  -> Request m         -- ^ The HTTP request.
makeProjectRequest projectId accessToken api method hostPath =
  appendHeaders
    [
      ("x-goog-project-id", projectId)
    ]
    (makeRequest accessToken api method hostPath)


-- | Class for Google API request.
class DoRequest a where
  -- | Perform a request.
  doRequest ::
       Request (ResourceT IO)  -- ^ The request.
    -> IO a                    -- ^ The action returning the result of performing the request.
  doRequest request =
    do
{--
      -- TODO: The following seems cleaner, but has type/instance problems:
      (_, manager) <- allocate (newManager def) closeManager
      doManagedRequest manager request
--}
      manager <- newManager def
      finally
        (doManagedRequest manager request)
        (closeManager manager)
  doManagedRequest ::
       Manager                 -- ^ The conduit HTTP manager.
    -> Request (ResourceT IO)  -- ^ The request.
    -> IO a                    -- ^ The action returning the result of performing the request.


instance DoRequest LBS8.ByteString where
  doManagedRequest manager request =
    do
      response <- runResourceT (httpLbs request manager)
      return $ responseBody response


instance DoRequest String where
  doManagedRequest manager request =
    do
      result <- doManagedRequest manager request
      return $ toString result


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
      result <- doManagedRequest manager request :: IO String
      return $ fromJust $ parseXMLDoc result


instance DoRequest JSValue where
  doManagedRequest manager request =
    do
      result <- doManagedRequest manager request :: IO String
      let
        Ok result' = decode result
      return result'


-- | Prepare a string for inclusion in a request.
makeRequestValue ::
     String          -- ^ The string.
  -> BS8.ByteString  -- ^ The prepared string.
-- TODO: In principle, we should UTF-8 encode the bytestrings packed below.
makeRequestValue = BS8.pack


-- | Prepare a name\/key for a header.
makeHeaderName ::
     String                -- ^ The name.
  -> CI.CI BS8.ByteString  -- ^ The prepared name.
-- TODO: In principle, we should UTF-8 encode the bytestrings packed below.
makeHeaderName = CI.mk . BS8.pack


-- | Prepare a value for a header.
makeHeaderValue ::
     String          -- ^ The value.
  -> BS8.ByteString  -- ^ The prepared value.
-- TODO: In principle, we should UTF-8 encode the bytestrings packed below.
makeHeaderValue = BS8.pack


-- | Append headers to a request.
appendHeaders ::
     [(String, String)]  -- ^ The (name\/key, value) pairs for the headers.
  -> Request m           -- ^ The request.
  -> Request m           -- ^ The request with the additional headers.
appendHeaders headers request =
  let
    headerize :: (String, String) -> (CI.CI BS8.ByteString, BS8.ByteString)
    headerize (n, v) = (makeHeaderName n, makeHeaderValue v)
  in
    request {
      requestHeaders = requestHeaders request ++ map headerize headers
    }


-- | Append a body to a request.
appendBody ::
     LBS8.ByteString  -- ^ The data for the body.
  -> Request m        -- ^ The request.
  -> Request m        -- ^ The request with the body appended.
appendBody bytes request =
  request {
    requestBody = RequestBodyLBS bytes
  }


-- | Append a query to a request.
appendQuery ::
     [(String, String)]  -- ^ The query keys and values.
  -> Request m           -- ^ The request.
  -> Request m           -- ^ The request with the query appended.
appendQuery query request =
  let
    makeParameter :: (String, String) -> String
    makeParameter (k, v) = k ++ "=" ++ urlEncode v
    query' :: String
    query' = intercalate "&" $ map makeParameter query
  in
    request
      {
        -- TODO: In principle, we should UTF-8 encode the bytestrings packed below.
        queryString = BS8.pack $ '?' : query'
      }
