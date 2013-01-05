-----------------------------------------------------------------------------
--
-- Module      :  Network.Google.Storage
-- Copyright   :  (c) 2012-13 Brian W Bush
-- License     :  MIT
--
-- Maintainer  :  Brian W Bush <b.w.bush@acm.org>
-- Stability   :  Stable
-- Portability :  Portable
--
-- |  Functions for the Google Storage API, see <https://developers.google.com/storage/docs/reference-methods>.

-----------------------------------------------------------------------------


module Network.Google.Storage (
-- * Types
  BucketName
, KeyName
, StorageAcl(..)
, MIMEType
-- * Service Requests
, getService
, getServiceUsingManager
-- * Bucket Requests
, putBucket
, putBucketUsingManager
, getBucket
, getBucketUsingManager
, deleteBucket
, deleteBucketUsingManager
-- * Object Requests
, getObject
, getObjectUsingManager
, putObject
, putObjectUsingManager
, headObject
, headObjectUsingManager
, deleteObject
, deleteObjectUsingManager
) where


import Control.Monad (liftM)
import Control.Monad.Trans.Resource (ResourceT)
import Crypto.MD5 (MD5Info, md5Base64)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.UTF8 (fromString)
import Data.ByteString.Lazy (ByteString)
import Data.List (intercalate, stripPrefix)
import Data.List.Util (separate)
import Data.Maybe (fromJust, isNothing, maybe)
import Network.Google (AccessToken, ProjectId, appendBody, appendHeaders, appendQuery, doManagedRequest, doRequest, makeProjectRequest)
import Network.HTTP.Base (urlEncode)
import Network.HTTP.Conduit (Manager, Request, queryString)
import Text.XML.Light (Element(elContent), QName(qName), filterChildName, ppTopElement, strContent)


-- | A bucket name.
type BucketName = String


-- | A key name for an object.
type KeyName = String


-- | Access control.
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


-- | MIME type.
type MIMEType = String


-- | The host name for API access.
storageHost :: String
storageHost = "storage.googleapis.com"


-- | The API version used here.
storageApi :: (String, String)
storageApi = ("x-goog-api-version", "2")


-- | Make a host name.
makeHost ::
     BucketName  -- ^ The bucket.
  -> String      -- ^ The host for the bucket.
makeHost bucket = bucket ++ "." ++ storageHost


-- | URL-encode a path.
makePath ::
     String  -- ^ The unencoded path.
  -> String  -- ^ The URL-encoded path.
-- TODO: Review whether the sequence of UTF-8 encoding and URL encoding is correct.  This works correctly with tests of exotic unicode sequences, however.
makePath = ('/' :) . intercalate "/" . map (urlEncode . unpack . fromString) . separate '/'


-- | List all of the buckets in a specified project.  This performs the \"GET Service\" request, see <https://developers.google.com/storage/docs/reference-methods#getservice>.
getService ::
     ProjectId    -- ^ The project ID.
  -> AccessToken  -- ^ The OAuth 2.0 access token.
  -> IO Element   -- ^ The action returning the XML with the metadata for the buckets.
getService = getServiceImpl doRequest


-- | List all of the buckets in a specified project.  This performs the \"GET Service\" request, see <https://developers.google.com/storage/docs/reference-methods#getservice>.
getServiceUsingManager ::
     Manager      -- ^ The conduit HTTP manager to use.
  -> ProjectId    -- ^ The project ID.
  -> AccessToken  -- ^ The OAuth 2.0 access token.
  -> IO Element   -- ^ The action returning the XML with the metadata for the buckets.
getServiceUsingManager = getServiceImpl . doManagedRequest


-- | List all of the buckets in a specified project.  This performs the \"GET Service\" request, see <https://developers.google.com/storage/docs/reference-methods#getservice>.
getServiceImpl ::
     (Request (ResourceT IO) -> IO Element)  -- ^ The function for performing the request.
  -> ProjectId                               -- ^ The project ID.
  -> AccessToken                             -- ^ The OAuth 2.0 access token.
  -> IO Element                              -- ^ The action returning the XML with the metadata for the buckets.
getServiceImpl doer projectId accessToken =
  do
    let
      request = makeProjectRequest projectId accessToken storageApi "GET" (storageHost, "/")
    doer request


-- | Creates a bucket in a specified project.  This performs the \"PUT Bucket\" request, see <https://developers.google.com/storage/docs/reference-methods#putbucket>.
putBucket ::
     ProjectId              -- ^ The project ID.
  -> StorageAcl             -- ^ The pre-defined access control.
  -> BucketName             -- ^ The bucket.
  -> AccessToken            -- ^ The OAuth 2.0 access token.
  -> IO [(String, String)]  -- ^ The action to put the object and return the response header.
putBucket = putBucketImpl doRequest


-- | Creates a bucket in a specified project.  This performs the \"PUT Bucket\" request, see <https://developers.google.com/storage/docs/reference-methods#putbucket>.
putBucketUsingManager ::
     Manager                -- ^ The conduit HTTP manager to use.
  -> ProjectId              -- ^ The project ID.
  -> StorageAcl             -- ^ The pre-defined access control.
  -> BucketName             -- ^ The bucket.
  -> AccessToken            -- ^ The OAuth 2.0 access token.
  -> IO [(String, String)]  -- ^ The action to put the object and return the response header.
putBucketUsingManager = putBucketImpl . doManagedRequest


-- | Creates a bucket in a specified project.  This performs the \"PUT Bucket\" request, see <https://developers.google.com/storage/docs/reference-methods#putbucket>.
putBucketImpl ::
     (Request (ResourceT IO) -> IO [(String, String)])  -- ^ The function for performing the request.
  -> ProjectId                                          -- ^ The project ID.
  -> StorageAcl                                         -- ^ The pre-defined access control.
  -> BucketName                                         -- ^ The bucket.
  -> AccessToken                                        -- ^ The OAuth 2.0 access token.
  -> IO [(String, String)]                              -- ^ The action to create the bucket and return the response header.
putBucketImpl doer projectId acl bucket accessToken =
  do
    let
      request = appendHeaders
        [
          ("x-goog-acl", show acl)
        ]
        (makeProjectRequest projectId accessToken storageApi "PUT" (makeHost bucket, "/"))
    doer request


-- | Lists the objects that are in a bucket.  This performs the \"GET Bucket\" request, see <https://developers.google.com/storage/docs/reference-methods#getbucket>.
getBucket ::
     ProjectId    -- ^ The project ID.
  -> BucketName   -- ^ The bucket.
  -> AccessToken  -- ^ The OAuth 2.0 access token.
  -> IO Element   -- ^ The action returning the XML with the metadata for the objects.
getBucket = getBucketImpl doRequest


-- | Lists the objects that are in a bucket.  This performs the \"GET Bucket\" request, see <https://developers.google.com/storage/docs/reference-methods#getbucket>.
getBucketUsingManager ::
     Manager      -- ^ The conduit HTTP manager to use.
  -> ProjectId    -- ^ The project ID.
  -> BucketName   -- ^ The bucket.
  -> AccessToken  -- ^ The OAuth 2.0 access token.
  -> IO Element   -- ^ The action returning the XML with the metadata for the objects.
getBucketUsingManager = getBucketImpl . doManagedRequest


-- | Lists the objects that are in a bucket.  This performs the \"GET Bucket\" request, see <https://developers.google.com/storage/docs/reference-methods#getbucket>.
getBucketImpl ::
     (Request (ResourceT IO) -> IO Element)  -- ^ The function for performing the request.
  -> ProjectId                               -- ^ The project ID.
  -> BucketName                              -- ^ The bucket.
  -> AccessToken                             -- ^ The OAuth 2.0 access token.
  -> IO Element                              -- ^ The action returning the XML with the metadata for the objects.
getBucketImpl doer projectId bucket accessToken =
  do
    results <- getBucketImpl' doer Nothing projectId bucket accessToken
    let
      root = head results
    return $ root {elContent = concatMap elContent results}


-- | Lists the objects that are in a bucket.  This performs the \"GET Bucket\" request, see <https://developers.google.com/storage/docs/reference-methods#getbucket>.
getBucketImpl' ::
     (Request (ResourceT IO) -> IO Element)  -- ^ The function for performing the request.
  -> Maybe KeyName                           -- ^ The key to start listing at.
  -> ProjectId                               -- ^ The project ID.
  -> BucketName                              -- ^ The bucket.
  -> AccessToken                             -- ^ The OAuth 2.0 access token.
  -> IO [Element]                            -- ^ The action returning the XML with the metadata for the objects.
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


-- | Deletes an empty bucket.  This performs the \"DELETE Bucket\" request, see <https://developers.google.com/storage/docs/reference-methods#deletebucket>.
deleteBucket ::
     ProjectId              -- ^ The project ID.
  -> BucketName             -- ^ The bucket.
  -> AccessToken            -- ^ The OAuth 2.0 access token.
  -> IO [(String, String)]  -- ^ The action to delete the bucket and return the response header.
deleteBucket = deleteBucketImpl doRequest


-- | Deletes an empty bucket.  This performs the \"DELETE Bucket\" request, see <https://developers.google.com/storage/docs/reference-methods#deletebucket>.
deleteBucketUsingManager ::
     Manager                -- ^ The conduit HTTP manager to use.
  -> ProjectId              -- ^ The project ID.
  -> BucketName             -- ^ The bucket.
  -> AccessToken            -- ^ The OAuth 2.0 access token.
  -> IO [(String, String)]  -- ^ The action to delete the bucket and return the response header.
deleteBucketUsingManager = deleteBucketImpl . doManagedRequest


-- | Deletes an empty bucket.  This performs the \"DELETE Bucket\" request, see <https://developers.google.com/storage/docs/reference-methods#deletebucket>.
deleteBucketImpl ::
     (Request (ResourceT IO) -> IO [(String, String)])  -- ^ The function for performing the request.
  -> ProjectId                                          -- ^ The project ID.
  -> BucketName                                         -- ^ The bucket.
  -> AccessToken                                        -- ^ The OAuth 2.0 access token.
  -> IO [(String, String)]                              -- ^ The action to delete the bucket and return the response header.
deleteBucketImpl doer  projectId bucket accessToken =
  do
    let
      request = makeProjectRequest projectId accessToken storageApi "DELETE" (makeHost bucket, "/")
    doer request


-- | Downloads an object.  This performs the \"GET Object\" request, see <https://developers.google.com/storage/docs/reference-methods#getobject>.
getObject ::
     ProjectId      -- ^ The project ID.
  -> BucketName     -- ^ The bucket.
  -> KeyName        -- ^ The object's key.
  -> AccessToken    -- ^ The OAuth 2.0 access token.
  -> IO ByteString  -- ^ The action returning the object.
getObject = getObjectImpl doRequest


-- | Downloads an object.  This performs the \"GET Object\" request, see <https://developers.google.com/storage/docs/reference-methods#getobject>.
getObjectUsingManager ::
     Manager        -- ^ The conduit HTTP manager to use.
  -> ProjectId      -- ^ The project ID.
  -> BucketName     -- ^ The bucket.
  -> KeyName        -- ^ The object's key.
  -> AccessToken    -- ^ The OAuth 2.0 access token.
  -> IO ByteString  -- ^ The action returning the object.
getObjectUsingManager = getObjectImpl . doManagedRequest


-- | Downloads an object.  This performs the \"GET Object\" request, see <https://developers.google.com/storage/docs/reference-methods#getobject>.
getObjectImpl ::
     (Request (ResourceT IO) -> IO ByteString)  -- ^ The function performing the action.
  -> ProjectId                                  -- ^ The project ID.
  -> BucketName                                 -- ^ The bucket.
  -> KeyName                                    -- ^ The object's key.
  -> AccessToken                                -- ^ The OAuth 2.0 access token.
  -> IO ByteString                              -- ^ The action returning the object.
getObjectImpl doer projectId bucket key accessToken =
  do
    let
      request = makeProjectRequest projectId accessToken storageApi "GET" (makeHost bucket, makePath key)
    doer request


-- TODO: Uploads objects by using HTML forms.  This performs the \"POST Object\" request, see <https://developers.google.com/storage/docs/reference-methods#postobject>.
postObject = undefined


-- | Uploads an object.  This performs the \"PUT Object\" request, see <https://developers.google.com/storage/docs/reference-methods#putobject>.
putObject ::
     ProjectId              -- ^ The project ID.
  -> StorageAcl             -- ^ The pre-defined access control.
  -> BucketName             -- ^ The bucket.
  -> KeyName                -- ^ The object's key.
  -> Maybe MIMEType         -- ^ The object's MIME type.
  -> ByteString             -- ^ The object's data.
  -> Maybe MD5Info          -- ^ The MD5 checksum.
  -> AccessToken            -- ^ The OAuth 2.0 access token.
  -> IO [(String, String)]  -- ^ The action to put the object and return the response header.
putObject = putObjectImpl doRequest


-- | Uploads an object.  This performs the \"PUT Object\" request, see <https://developers.google.com/storage/docs/reference-methods#putobject>.
putObjectUsingManager ::
     Manager                -- ^ The conduit HTTP manager to use.
  -> ProjectId              -- ^ The project ID.
  -> StorageAcl             -- ^ The pre-defined access control.
  -> BucketName             -- ^ The bucket.
  -> KeyName                -- ^ The object's key.
  -> Maybe MIMEType         -- ^ The object's MIME type.
  -> ByteString             -- ^ The object's data.
  -> Maybe MD5Info          -- ^ The MD5 checksum.
  -> AccessToken            -- ^ The OAuth 2.0 access token.
  -> IO [(String, String)]  -- ^ The action to put the object and return the response header.
putObjectUsingManager = putObjectImpl . doManagedRequest


-- | Uploads an object.  This performs the \"PUT Object\" request, see <https://developers.google.com/storage/docs/reference-methods#putobject>.
putObjectImpl ::
     (Request (ResourceT IO) -> IO [(String, String)])  -- ^ The function for performing the request.
  -> ProjectId                                          -- ^ The project ID.
  -> StorageAcl                                         -- ^ The pre-defined access control.
  -> BucketName                                         -- ^ The bucket.
  -> KeyName                                            -- ^ The object's key.
  -> Maybe MIMEType                                     -- ^ The object's MIME type.
  -> ByteString                                         -- ^ The object's data.
  -> Maybe MD5Info                                      -- ^ The MD5 checksum.
  -> AccessToken                                        -- ^ The OAuth 2.0 access token.
  -> IO [(String, String)]                              -- ^ The action to put the object and return the response header.
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


-- | Lists metadata for an object.  This performs the \"HEAD Object\" request, see <https://developers.google.com/storage/docs/reference-methods#headobject>.
headObject ::
     ProjectId              -- ^ The project ID.
  -> BucketName             -- ^ The bucket.
  -> KeyName                -- ^ The object's key.
  -> AccessToken            -- ^ The OAuth 2.0 access token.
  -> IO [(String, String)]  -- ^ The action returning the object's metadata.
headObject = headObjectImpl doRequest


-- | Lists metadata for an object.  This performs the \"HEAD Object\" request, see <https://developers.google.com/storage/docs/reference-methods#headobject>.
headObjectUsingManager ::
     Manager                -- ^ The conduit HTTP manager to use.
  -> ProjectId              -- ^ The project ID.
  -> BucketName             -- ^ The bucket.
  -> KeyName                -- ^ The object's key.
  -> AccessToken            -- ^ The OAuth 2.0 access token.
  -> IO [(String, String)]  -- ^ The action returning the object's metadata.
headObjectUsingManager = headObjectImpl . doManagedRequest


-- | Lists metadata for an object.  This performs the \"HEAD Object\" request, see <https://developers.google.com/storage/docs/reference-methods#headobject>.
headObjectImpl ::
     (Request (ResourceT IO) -> IO [(String, String)])  -- ^ The function for performing the request.
  -> ProjectId                                          -- ^ The project ID.
  -> BucketName                                         -- ^ The bucket.
  -> KeyName                                            -- ^ The object's key.
  -> AccessToken                                        -- ^ The OAuth 2.0 access token.
  -> IO [(String, String)]                              -- ^ The action returning the object's metadata.
headObjectImpl doer projectId bucket key accessToken =
  do
    let
      request = makeProjectRequest projectId accessToken storageApi "HEAD" (makeHost bucket, makePath key)
    doer request


-- | Deletes an object.  This performs the \"DELETE Object\" request, see <https://developers.google.com/storage/docs/reference-methods#deleteobject>.
deleteObject ::
     ProjectId              -- ^ The project ID.
  -> BucketName             -- ^ The bucket.
  -> KeyName                -- ^ The object's key.
  -> AccessToken            -- ^ The OAuth 2.0 access token.
  -> IO [(String, String)]  -- ^ The action to delete the object and return the response header.
deleteObject = deleteObjectImpl doRequest


-- | Deletes an object.  This performs the \"DELETE Object\" request, see <https://developers.google.com/storage/docs/reference-methods#deleteobject>.
deleteObjectUsingManager ::
     Manager                -- ^ The conduit HTTP manager to use.
  -> ProjectId              -- ^ The project ID.
  -> BucketName             -- ^ The bucket.
  -> KeyName                -- ^ The object's key.
  -> AccessToken            -- ^ The OAuth 2.0 access token.
  -> IO [(String, String)]  -- ^ The action to delete the object and return the response header.
deleteObjectUsingManager = deleteObjectImpl . doManagedRequest


-- | Deletes an object.  This performs the \"DELETE Object\" request, see <https://developers.google.com/storage/docs/reference-methods#deleteobject>.
deleteObjectImpl ::
     (Request (ResourceT IO) -> IO [(String, String)])  -- ^ The function for performing the request.
  -> ProjectId                                          -- ^ The project ID.
  -> BucketName                                         -- ^ The bucket.
  -> KeyName                                            -- ^ The object's key.
  -> AccessToken                                        -- ^ The OAuth 2.0 access token.
  -> IO [(String, String)]                              -- ^ The action to delete the object and return the response header.
deleteObjectImpl doer projectId bucket key accessToken =
  do
    let
      request = makeProjectRequest projectId accessToken storageApi "DELETE" (makeHost bucket, makePath key)
    doer request
