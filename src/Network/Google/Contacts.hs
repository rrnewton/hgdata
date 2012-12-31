-----------------------------------------------------------------------------
--
-- Module      :  Network.Google.Contacts
-- Copyright   :  (c) 2012-13 Brian W Bush
-- License     :  MIT
--
-- Maintainer  :  Brian W Bush <b.w.bush@acm.org>
-- Stability   :  Stable
-- Portability :  Portable.
--
-- |
--
-----------------------------------------------------------------------------


module Network.Google.Contacts (
-- * Functions
  listContacts
, extractPasswords
) where


import Control.Monad ((<=<), (>>), liftM)
import Crypto.GnuPG (Recipient, decrypt, encrypt)
import Data.ByteString.Util (lbsToS)
import Data.List (stripPrefix)
import Data.Maybe (catMaybes, fromJust)
import Network.Google (AccessToken, doRequest, makeRequest, makeRequestValue)
import Network.HTTP.Conduit (Request(..), def, httpLbs, responseBody, withManager)
import Text.XML.Light (Element, elChildren, filterChildName, parseXMLDoc, qName, strContent)


-- | The host for API access.
contactsHost :: String
contactsHost = "www.google.com"


-- | The API version used here.
contactsApi :: (String, String)
contactsApi = ("Gdata-version", "3.0")


-- | List the contacts.
listContacts ::
     AccessToken  -- ^ The OAuth 2.0 access token.
  -> IO Element   -- ^ The action returning the contacts in XML format.
listContacts accessToken =
  do
    let
      request = listContactsRequest accessToken
    doRequest request


-- | Make an HTTP request to list the contacts.
listContactsRequest ::
     AccessToken  -- ^ The OAuth 2.0 access token.
  -> Request m    -- ^ The request.
listContactsRequest accessToken =
  (makeRequest accessToken contactsApi "GET" (contactsHost, "/m8/feeds/contacts/default/full/"))
  {
    queryString = makeRequestValue "?max-results=100000"
  }


-- | Extract the passwords from a contact list.  Passwords are re-encrypted if recipients for the re-encrypted list are specified.
extractPasswords ::
     [Recipient]  -- ^ The recipients to re-encrypt the passwords to.
  -> String       -- ^ The contact list.
  -> IO String    -- ^ The action return the decrypted and then possibly re-encrypted passwords.
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


-- | Extract the passwords from a contact list.
extractPasswords' ::
     String                      -- ^ The contact list.
  -> [(String, String, String)]  -- ^ The contacts in (title, organization, GnuPG text) format.
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
