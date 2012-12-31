-----------------------------------------------------------------------------
--
-- Module      :  Network.Google.Storage.Encrypted
-- Copyright   :  (c) 2012-13 Brian W Bush
-- License     :  MIT
--
-- Maintainer  :  Brian W Bush <b.w.bush@acm.org>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Functions for putting and getting GnuPG-encrypted objects in Google Storage.
--
-----------------------------------------------------------------------------


module Network.Google.Storage.Encrypted (
  -- * Object Requests
  putEncryptedObject
, putEncryptedObjectUsingManager
, getEncryptedObject
, getEncryptedObjectUsingManager
) where


import Crypto.GnuPG (Recipient, decryptLbs, encryptLbs)
import Crypto.MD5 (MD5Info)
import Data.ByteString.Lazy (ByteString)
import Network.Google (AccessToken, ProjectId)
import Network.Google.Storage (BucketName, KeyName, MIMEType, StorageAcl, getObject, getObjectUsingManager, putObject, putObjectUsingManager)
import Network.HTTP.Conduit (Manager)


-- | Downloads an object and decrypts it.
getEncryptedObject ::
     ProjectId              -- ^ The project ID.
  -> BucketName             -- ^ The bucket.
  -> KeyName                -- ^ The object's key.
  -> AccessToken            -- ^ The OAuth 2.0 access token.
  -> IO ByteString          -- ^ The action returning the object.
getEncryptedObject = getEncryptedObjectImpl getObject


-- | Downloads an object and decrypts it.
getEncryptedObjectUsingManager ::
     Manager                -- ^ The conduit HTTP manager to use.
  -> ProjectId              -- ^ The project ID.
  -> BucketName             -- ^ The bucket.
  -> KeyName                -- ^ The object's key.
  -> AccessToken            -- ^ The OAuth 2.0 access token.
  -> IO ByteString          -- ^ The action returning the object.
getEncryptedObjectUsingManager = getEncryptedObjectImpl . getObjectUsingManager


-- | Downloads an object and decrypts it.
getEncryptedObjectImpl ::
     (String -> String -> String -> AccessToken -> IO ByteString)  -- ^ Function for getting an object.
  -> ProjectId                                                     -- ^ The project ID.
  -> BucketName                                                    -- ^ The bucket.
  -> KeyName                                                       -- ^ The object's key.
  -> AccessToken                                                   -- ^ The OAuth 2.0 access token.
  -> IO ByteString                                                 -- ^ The action returning the object.
getEncryptedObjectImpl getter projectId bucket key accessToken =
  do
    bytes <- getter projectId bucket key accessToken
    decryptLbs $ bytes


-- | Encrypt an object and upload it.
putEncryptedObject ::
     [Recipient]            -- ^ The recipients for GnuPG encryption of the uploaded files.
  -> ProjectId              -- ^ The project ID.
  -> StorageAcl             -- ^ The pre-defined access control.
  -> BucketName             -- ^ The bucket.
  -> KeyName                -- ^ The object's key.
  -> Maybe MIMEType         -- ^ The object's MIME type.
  -> ByteString             -- ^ The object's data.
  -> Maybe MD5Info          -- ^ The MD5 checksum.
  -> AccessToken            -- ^ The OAuth 2.0 access token.
  -> IO [(String, String)]  -- ^ The action to put the object and return the response header.
putEncryptedObject = putEncryptedObjectImpl putObject


putEncryptedObjectUsingManager ::
     Manager                -- ^ The conduit HTTP manager to use.
  -> [Recipient]            -- ^ The recipients for GnuPG encryption of the uploaded files.
  -> ProjectId              -- ^ The project ID.
  -> StorageAcl             -- ^ The pre-defined access control.
  -> BucketName             -- ^ The bucket.
  -> KeyName                -- ^ The object's key.
  -> Maybe MIMEType         -- ^ The object's MIME type.
  -> ByteString             -- ^ The object's data.
  -> Maybe MD5Info          -- ^ The MD5 checksum.
  -> AccessToken            -- ^ The OAuth 2.0 access token.
  -> IO [(String, String)]  -- ^ The action to put the object and return the response header.
putEncryptedObjectUsingManager = putEncryptedObjectImpl . putObjectUsingManager


putEncryptedObjectImpl ::
     (String -> StorageAcl -> String -> String -> Maybe String -> ByteString -> Maybe (String, String) -> AccessToken -> IO [(String, String)])  -- ^ Function for putting an object.
  -> [Recipient]                                                                                                                                 -- ^ The recipients for GnuPG encryption of the uploaded files.
  -> ProjectId                                                                                                                                   -- ^ The project ID.
  -> StorageAcl                                                                                                                                  -- ^ The pre-defined access control.
  -> BucketName                                                                                                                                  -- ^ The bucket.
  -> KeyName                                                                                                                                     -- ^ The object's key.
  -> Maybe MIMEType                                                                                                                              -- ^ The object's MIME type.
  -> ByteString                                                                                                                                  -- ^ The object's data.
  -> Maybe MD5Info                                                                                                                               -- ^ The MD5 checksum.
  -> AccessToken                                                                                                                                 -- ^ The OAuth 2.0 access token.
  -> IO [(String, String)]                                                                                                                       -- ^ The action to put the object and return the response header.
putEncryptedObjectImpl putter recipients projectId acl bucket key _ bytes _ accessToken =
  do
    bytes' <- encryptLbs recipients bytes
    putter projectId acl bucket key (Just "application/pgp-encrypted") bytes' Nothing accessToken
