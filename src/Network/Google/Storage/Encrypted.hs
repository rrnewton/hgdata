-----------------------------------------------------------------------------
--
-- Module      :  Network.Google.Storage.Encrypted
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


module Network.Google.Storage.Encrypted (
  getEncryptedObject
, getEncryptedObjectUsingManager
, putEncryptedObject
, putEncryptedObjectUsingManager
) where


import Crypto.GnuPG (decryptLbs, encryptLbs)
import Data.ByteString.Lazy (ByteString)
import Network.Google (AccessToken)
import Network.Google.Storage (StorageAcl, getObject, getObjectUsingManager, putObject, putObjectUsingManager)
import Network.HTTP.Conduit (Manager)


getEncryptedObject :: String -> String -> String -> AccessToken -> IO ByteString
getEncryptedObject = getEncryptedObjectImpl getObject


getEncryptedObjectUsingManager :: Manager -> String -> String -> String -> AccessToken -> IO ByteString
getEncryptedObjectUsingManager = getEncryptedObjectImpl . getObjectUsingManager


getEncryptedObjectImpl :: (String -> String -> String -> AccessToken -> IO ByteString) -> String -> String -> String -> AccessToken -> IO ByteString
getEncryptedObjectImpl getter projectId bucket key accessToken =
  do
    bytes <- getter projectId bucket key accessToken
    decryptLbs $ bytes


putEncryptedObject :: [String] -> String -> StorageAcl -> String -> String -> Maybe String -> ByteString -> AccessToken -> IO [(String, String)]
putEncryptedObject = putEncryptedObjectImpl putObject


putEncryptedObjectUsingManager :: Manager -> [String] -> String -> StorageAcl -> String -> String -> Maybe String -> ByteString -> AccessToken -> IO [(String, String)]
putEncryptedObjectUsingManager = putEncryptedObjectImpl . putObjectUsingManager


putEncryptedObjectImpl :: (String -> StorageAcl -> String -> String -> Maybe String -> ByteString -> AccessToken -> IO [(String, String)]) -> [String] -> String -> StorageAcl -> String -> String -> Maybe String -> ByteString -> AccessToken -> IO [(String, String)]
putEncryptedObjectImpl putter recipients projectId acl bucket key _ bytes accessToken =
  do
    bytes' <- encryptLbs recipients bytes
    putter projectId acl bucket key (Just "application/pgp-encrypted") bytes' accessToken
