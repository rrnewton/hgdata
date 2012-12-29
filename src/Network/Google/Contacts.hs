-----------------------------------------------------------------------------
--
-- Module      :  Network.Google.Contacts
-- Copyright   :  (c) 2012-13 Brian W Bush
-- License     :  MIT
--
-- Maintainer  :  Brian W Bush <b.w.bush@acm.org>
-- Stability   :  Stable
-- Portability :  Linux
--
-- |
--
-----------------------------------------------------------------------------


module Network.Google.Contacts (
  extractPasswords
, listContacts
) where


import Control.Monad ((<=<), (>>), liftM)
import Crypto.GnuPG (decrypt, encrypt)
import Data.ByteString.Util (lbsToS)
import Data.List (stripPrefix)
import Data.Maybe (catMaybes, fromJust)
import Network.Google (AccessToken, doRequest, makeRequest, makeRequestValue)
import Network.HTTP.Conduit (Request(..), def, httpLbs, responseBody, withManager)
import Text.XML.Light (Element, elChildren, filterChildName, parseXMLDoc, qName, strContent)


contactsHost :: String
contactsHost = "www.google.com"


contactsApi :: (String, String)
contactsApi = ("Gdata-version", "3.0")


listContacts :: AccessToken -> IO Element
listContacts accessToken =
  do
    let
      request = listContactsRequest accessToken
    doRequest request


listContactsRequest :: AccessToken -> Request m
listContactsRequest accessToken =
  (makeRequest accessToken contactsApi "GET" (contactsHost, "/m8/feeds/contacts/default/full/"))
  {
    queryString = makeRequestValue "?max-results=100000"
  }


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
    (if null recipients then return . id else encrypt recipients) $ unlines passwords'


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
