-----------------------------------------------------------------------------
--
-- Module      :  Network.Google.Contacts
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


module Network.Google.Contacts (
  extractPasswords
, listContacts
) where


import Control.Monad ((>>), (<=<), liftM)
import Data.List (stripPrefix)
import Data.Maybe (catMaybes, fromJust, isJust)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Crypto.GnuPG
import Network.Google (AccessToken, makeRequest)
import Network.HTTP.Conduit (Request, def, httpLbs, responseBody, withManager)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Text.XML.Light --(Element, filterChildren, parseXMLDoc)


listContacts :: AccessToken -> IO String
listContacts accessToken =
  do
    let
      lbsToS :: LBS.ByteString -> String
      lbsToS = T.unpack . T.decodeUtf8 . BS.concat . LBS.toChunks
      request = listContactsRequest accessToken
    response <- withManager $ httpLbs request
    return $ lbsToS $ responseBody response


listContactsRequest :: AccessToken -> Request m
listContactsRequest = makeRequest "/m8/feeds/contacts/default/full/"


extractPasswords :: [String] -> String -> IO String
extractPasswords recipients text =
  do
    let
      passwords = extractPasswords' text
      replacePassword (t, o, p) =
        do
          p' <- decrypt p
          return $ unlines $ ["-----", "", t, o, "", p']
    passwords' <- mapM replacePassword passwords
    encrypt recipients $ unlines passwords'


extractPasswords' :: String -> [(String, String, String)]
extractPasswords' text =
  let
    findChildName :: String -> Element -> Maybe Element
    findChildName x = filterChildName (\z -> qName z == x)
    checkPrefix :: String -> String -> Maybe String
    checkPrefix p x = liftM (const x) . stripPrefix p $ x
    getTitle :: Element -> Maybe String
    getTitle = liftM strContent . findChildName "title"
    getOrganization :: Element -> Maybe String
    getOrganization = liftM strContent . findChildName "orgName" <=< findChildName "organization"
    getPGP :: Element -> Maybe String
    getPGP = checkPrefix "-----BEGIN PGP MESSAGE-----" <=< liftM strContent . findChildName "content"
    getEntry :: Element -> Maybe (String, String, String)
    getEntry x =
      do
        t <- getTitle x
        o <- getOrganization x
        p <- getPGP x
        return (t, o, p)
    xml :: Element
    xml = fromJust $ parseXMLDoc text
  in
    catMaybes $ map getEntry $ elChildren xml
