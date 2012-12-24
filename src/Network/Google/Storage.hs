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
  StorageAcl
, deleteBucket
, deleteObject
, getBucket
, getObject
, getService
, headObject
, putBucket
, putObject
) where


import Crypto.MD5 (md5Base64)
import Data.ByteString.Lazy (ByteString)
import Data.List (stripPrefix)
import Data.Maybe (maybe)
import Network.Google (AccessToken, appendBody, appendHeaders, doRequest, makeProjectRequest)
import Text.XML.Light (Element)


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
makePath key = "/" ++ key


getService :: String -> AccessToken -> IO Element
getService projectId accessToken =
  do
    let
      request = makeProjectRequest projectId accessToken storageApi "GET" (storageHost, "/")
    doRequest request


putBucket :: String -> AccessToken -> StorageAcl -> String -> IO [(String, String)]
putBucket projectId accessToken acl bucket =
  do
    let
      request = appendHeaders
        [
          ("x-goog-acl", show acl)
        ]
        (makeProjectRequest projectId accessToken storageApi "PUT" (makeHost bucket, "/"))
    doRequest request


getBucket :: String -> AccessToken -> String -> IO Element
getBucket projectId accessToken bucket =
  do
    let
      request =
        appendHeaders
          [
            ("max-keys", "1000000")
          ]
          (makeProjectRequest projectId accessToken storageApi "GET" (makeHost bucket, "/"))
    doRequest request


deleteBucket :: String -> AccessToken -> String -> IO [(String, String)]
deleteBucket projectId accessToken bucket =
  do
    let
      request = makeProjectRequest projectId accessToken storageApi "DELETE" (makeHost bucket, "/")
    doRequest request


getObject :: String -> AccessToken -> String -> String -> IO ByteString
getObject projectId accessToken bucket key =
  do
    let
      request = (makeProjectRequest projectId accessToken storageApi "GET" (makeHost bucket, makePath key))
    doRequest request


postObject :: String -> AccessToken -> String -> String -> StorageAcl -> String -> ByteString -> IO [(String, String)]
postObject = undefined


putObject :: String -> AccessToken -> String -> String -> StorageAcl -> Maybe String -> ByteString -> IO [(String, String)]
putObject projectId accessToken bucket key acl mimeType bytes =
  do
    let
      request =
        appendBody bytes $
        appendHeaders (
          [
            ("x-goog-acl", show acl)
          , ("Content-MD5", md5Base64 bytes)
          ]
          ++
          maybe [] (\x -> [("Content-Type", x)]) mimeType
        ) (makeProjectRequest projectId accessToken storageApi "PUT" (makeHost bucket, makePath key))
    doRequest request


headObject :: String -> AccessToken -> String -> String -> IO [(String, String)]
headObject projectId accessToken bucket key =
  do
    let
      request = (makeProjectRequest projectId accessToken storageApi "HEAD" (makeHost bucket, makePath key))
    doRequest request


deleteObject :: String -> AccessToken -> String -> String -> IO [(String, String)]
deleteObject projectId accessToken bucket key =
  do
    let
      request = (makeProjectRequest projectId accessToken storageApi "DELETE" (makeHost bucket, makePath key))
    doRequest request
