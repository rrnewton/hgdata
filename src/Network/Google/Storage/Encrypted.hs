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
, putEncryptedObject
) where


import Crypto.GnuPG (decryptLbs, encryptLbs)
import Data.ByteString.Lazy (ByteString)
import Network.Google (AccessToken)
import Network.Google.Storage (StorageAcl, getObject, putObject)


getEncryptedObject :: String -> String -> String -> AccessToken -> IO ByteString
getEncryptedObject projectId bucket key accessToken =
  do
    bytes <- getObject projectId bucket key accessToken
    decryptLbs $ bytes


putEncryptedObject :: [String] -> String -> StorageAcl -> String -> String -> Maybe String -> ByteString -> AccessToken -> IO [(String, String)]
putEncryptedObject recipients projectId acl bucket key _ bytes accessToken =
  do
    bytes' <- encryptLbs recipients bytes
    putObject projectId acl bucket key (Just "application/pgp-encrypted") bytes' accessToken
