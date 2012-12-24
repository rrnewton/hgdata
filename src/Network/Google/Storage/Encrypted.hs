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


getEncryptedObject :: String -> AccessToken -> String -> String -> IO ByteString
getEncryptedObject projectId accessToken bucket key =
  do
    bytes <- getObject projectId accessToken bucket key
    decryptLbs $ bytes


putEncryptedObject :: [String] -> String -> AccessToken -> String -> String -> StorageAcl -> Maybe String -> ByteString -> IO [(String, String)]
putEncryptedObject recipients projectId accessToken bucket key acl _ bytes =
  do
    bytes' <- encryptLbs recipients bytes
    putObject projectId accessToken bucket key acl (Just "application/pgp-encrypted") bytes'
