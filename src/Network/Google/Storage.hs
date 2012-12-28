-----------------------------------------------------------------------------
--
-- Module      :  Network.Google.Storage
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-- https://developers.google.com/storage/docs/reference-methods

-----------------------------------------------------------------------------


module Network.Google.Storage (
  StorageAcl(..)
, deleteBucket
, deleteBucketUsingManager
, deleteObject
, deleteObjectUsingManager
, getBucket
, getBucketUsingManager
, getObject
, getObjectUsingManager
, getService
, getServiceUsingManager
, headObject
, headObjectUsingManager
, putBucket
, putBucketUsingManager
, putObject
, putObjectUsingManager
) where


import Control.Monad (liftM)
import Control.Monad.Trans.Resource (ResourceT)
import Crypto.MD5 (md5Base64)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Util (sToBs)
import Data.List (intersperse, stripPrefix)
import Data.List.Util (separate)
import Data.Maybe (fromJust, isNothing, maybe)
import Network.Google (AccessToken, appendBody, appendHeaders, appendQuery, doManagedRequest, doRequest, makeProjectRequest)
import Network.HTTP.Base (urlEncode)
import Network.HTTP.Conduit (Manager, Request, queryString)
import Text.XML.Light (Element(elContent), QName(qName), filterChildName, ppTopElement, strContent)


data StorageAcl =
    Private
  | PublicRead
  | PublicReadWrite
  | AuthenicatedRead
  | BucketOwnerRead
  | BucketOwnerFullControl
    deriving (Bounded, Enum, Eq)

instance Show StorageAcl where
  show Private = "private"
  show PublicRead = "public-read"
  show PublicReadWrite = "public-read-write"
  show AuthenicatedRead = "authenticated-read"
  show BucketOwnerRead = "bucket-owner-read"
  show BucketOwnerFullControl = "bucket-owner-full-control"

instance Read StorageAcl where
  readsPrec _ x =
    let
      matches :: [StorageAcl]
      matches = filter (\y -> show y == x) [minBound..maxBound]
    in
      if null matches
        then []
        else [(last matches, "")]


storageHost :: String
storageHost = "storage.googleapis.com"


storageApi :: (String, String)
storageApi = ("x-goog-api-version", "2")


makeHost :: String -> String
makeHost bucket = bucket ++ "." ++ storageHost


makePath :: String -> String
makePath = ('/' :) . concat . intersperse "/" . map (urlEncode . unpack . sToBs) . separate '/'


getService :: String -> AccessToken -> IO Element
getService = getServiceImpl doRequest


getServiceUsingManager :: Manager -> String -> AccessToken -> IO Element
getServiceUsingManager = getServiceImpl . doManagedRequest


getServiceImpl :: (Request (ResourceT IO) -> IO Element) -> String -> AccessToken -> IO Element
getServiceImpl doer projectId accessToken =
  do
    let
      request = makeProjectRequest projectId accessToken storageApi "GET" (storageHost, "/")
    doer request


putBucket :: String -> StorageAcl -> String -> AccessToken -> IO [(String, String)]
putBucket = putBucketImpl doRequest


putBucketUsingManager :: Manager -> String -> StorageAcl -> String -> AccessToken -> IO [(String, String)]
putBucketUsingManager = putBucketImpl . doManagedRequest


putBucketImpl :: (Request (ResourceT IO) -> IO [(String, String)]) -> String -> StorageAcl -> String -> AccessToken -> IO [(String, String)]
putBucketImpl doer projectId acl bucket accessToken =
  do
    let
      request = appendHeaders
        [
          ("x-goog-acl", show acl)
        ]
        (makeProjectRequest projectId accessToken storageApi "PUT" (makeHost bucket, "/"))
    doer request


getBucket :: String -> String -> AccessToken -> IO Element
getBucket = getBucketImpl doRequest


getBucketUsingManager :: Manager -> String -> String -> AccessToken -> IO Element
getBucketUsingManager = getBucketImpl . doManagedRequest


getBucketImpl :: (Request (ResourceT IO) -> IO Element) -> String -> String -> AccessToken -> IO Element
getBucketImpl doer projectId bucket accessToken =
  do
    results <- getBucketImpl' doer Nothing projectId bucket accessToken
    let
      root = head results
    return $ root {elContent = concat $ map elContent results}


getBucketImpl' :: (Request (ResourceT IO) -> IO Element) -> Maybe String -> String -> String -> AccessToken -> IO [Element]
getBucketImpl' doer marker projectId bucket accessToken =
  do
    let
      request =
        appendHeaders
          [
            ("max-keys", "1000000")
          ]
          (makeProjectRequest projectId accessToken storageApi "GET" (makeHost bucket, "/"))
      request' = maybe request (\x -> appendQuery [("marker", x)] request) marker
    result <- doer request'
    let
      marker' :: Maybe String
      marker' = liftM strContent $ filterChildName (("NextMarker" ==) . qName) result
    if isNothing marker' || marker' == Just ""
      then return [result]
      else liftM (result :) $ getBucketImpl' doer marker' projectId bucket accessToken


deleteBucket :: String -> String -> AccessToken -> IO [(String, String)]
deleteBucket = deleteBucketImpl doRequest


deleteBucketUsingManager :: Manager -> String -> String -> AccessToken -> IO [(String, String)]
deleteBucketUsingManager = deleteBucketImpl . doManagedRequest


deleteBucketImpl :: (Request (ResourceT IO) -> IO [(String, String)]) -> String -> String -> AccessToken -> IO [(String, String)]
deleteBucketImpl doer  projectId bucket accessToken =
  do
    let
      request = makeProjectRequest projectId accessToken storageApi "DELETE" (makeHost bucket, "/")
    doer request


getObject :: String -> String -> String -> AccessToken -> IO ByteString
getObject = getObjectImpl doRequest


getObjectUsingManager :: Manager -> String -> String -> String -> AccessToken -> IO ByteString
getObjectUsingManager = getObjectImpl . doManagedRequest


getObjectImpl :: (Request (ResourceT IO) -> IO ByteString) -> String -> String -> String -> AccessToken -> IO ByteString
getObjectImpl doer projectId bucket key accessToken =
  do
    let
      request = (makeProjectRequest projectId accessToken storageApi "GET" (makeHost bucket, makePath key))
    doer request


postObject = undefined


putObject :: String -> StorageAcl -> String -> String -> Maybe String -> ByteString -> Maybe (String, String) -> AccessToken -> IO [(String, String)]
putObject = putObjectImpl doRequest


putObjectUsingManager :: Manager -> String -> StorageAcl -> String -> String -> Maybe String -> ByteString -> Maybe (String, String) -> AccessToken -> IO [(String, String)]
putObjectUsingManager = putObjectImpl . doManagedRequest


putObjectImpl :: (Request (ResourceT IO) -> IO [(String, String)]) -> String -> StorageAcl -> String -> String -> Maybe String -> ByteString -> Maybe (String, String) -> AccessToken -> IO [(String, String)]
putObjectImpl doer projectId acl bucket key mimeType bytes md5 accessToken =
  do
    let
      md5' = maybe (snd $ md5Base64 bytes) snd md5
      request =
        appendBody bytes $
        appendHeaders (
          [
            ("x-goog-acl", show acl)
          , ("Content-MD5", md5')
          ]
          ++
          maybe [] (\x -> [("Content-Type", x)]) mimeType
        ) (makeProjectRequest projectId accessToken storageApi "PUT" (makeHost bucket, makePath key))
    doer request


headObject :: String -> String -> String -> AccessToken -> IO [(String, String)]
headObject = headObjectImpl doRequest


headObjectUsingManager :: Manager -> String -> String -> String -> AccessToken -> IO [(String, String)]
headObjectUsingManager = headObjectImpl . doManagedRequest


headObjectImpl :: (Request (ResourceT IO) -> IO [(String, String)]) -> String -> String -> String -> AccessToken -> IO [(String, String)]
headObjectImpl doer projectId bucket key accessToken =
  do
    let
      request = (makeProjectRequest projectId accessToken storageApi "HEAD" (makeHost bucket, makePath key))
    doer request


deleteObject :: String -> String -> String -> AccessToken -> IO [(String, String)]
deleteObject = deleteObjectImpl doRequest


deleteObjectUsingManager :: Manager -> String -> String -> String -> AccessToken -> IO [(String, String)]
deleteObjectUsingManager = deleteObjectImpl . doManagedRequest


deleteObjectImpl :: (Request (ResourceT IO) -> IO [(String, String)]) -> String -> String -> String -> AccessToken -> IO [(String, String)]
deleteObjectImpl doer projectId bucket key accessToken =
  do
    let
      request = (makeProjectRequest projectId accessToken storageApi "DELETE" (makeHost bucket, makePath key))
    doer request
