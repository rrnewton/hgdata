-----------------------------------------------------------------------------
--
-- Module      :  Network.Google.Bookmarks
-- Copyright   :  (c) 2013 Brian W Bush
-- License     :  MIT
--
-- Maintainer  :  Brian W Bush <b.w.bush@acm.org>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Functions for accessing the unofficial Google Bookmarks API, see <http://www.mmartins.com/mmartins/googlebookmarksapi/>.
--
-----------------------------------------------------------------------------


module Network.Google.Bookmarks (
-- * Types
  EMail
, Password
, SmsToken
-- * Functions
, listBookmarks
) where


import Control.Monad (liftM)
import Data.ByteString.Char8 as BS8 (ByteString, pack, unpack)
import Data.ByteString.Lazy.Char8 as LBS8 (ByteString, pack, unpack)
import Data.ByteString.Lazy.UTF8 (fromString, toString)
import Data.Maybe (fromJust)
import Data.Time.Clock (getCurrentTime)
import Network.Google (appendHeaders)
import Network.HTTP.Conduit (CookieJar, Request(..), RequestBody(..), Response(..), def, httpLbs, parseUrl, withManager)
import Text.XML.Light (Element(..), QName(..), blank_name, filterElement, findAttr, parseXMLDoc)


-- | Google e-mail address.
type EMail = String


-- | Google password.
type Password = String


-- | SMS authentication token.
type SmsToken = String


-- | List the bookmarks, see <http://www.mmartins.com/mmartins/googlebookmarksapi/>.
listBookmarks ::
     EMail       -- ^ The Google e-mail address.
  -> Password    -- ^ The Google password.
  -> SmsToken    -- ^ The SMS authentication token.
  -> IO Element  -- ^ The action returning the bookmarks in XML format.
listBookmarks email password smsToken =
  do
    now <- getCurrentTime
    withManager $ \manager -> do
      requestGet1 <- parseUrl $ "https://accounts.google.com/Login?continue=" ++ listingUrl ++ "&hl=en&service=bookmarks&authuser=0"
      responseGet1 <- httpLbs requestGet1 manager
      let
        encode = LBS8.unpack . fromString
        responseXml = fromJust . parseXMLDoc . toString . responseBody
        bodyGet1 = responseXml responseGet1
        cookieJarGet1 = responseCookieJar responseGet1
        requestPost1 =
          (accountsPostRequest "/ServiceLoginAuth") {
            requestBody = RequestBodyBS $ BS8.pack $
            "continue=" ++ listingUrl
            ++ "&service=bookmarks"
            ++ "&dsh=" ++ extractValue "dsh" bodyGet1
            ++ "&GALX=" ++ extractValue "GALX" bodyGet1
            ++ "&bgresponse=js_disabled"
            ++ "&Email=" ++ encode email
            ++ "&Passwd=" ++ encode password
            ++ "&PersistentCookie=yes"
          , cookieJar = Just cookieJarGet1
          , redirectCount = 0
          , checkStatus = \_ _ _ -> Nothing
          }
      responsePost1 <- httpLbs requestPost1 manager
      let
        cookieJarPost1 = responseCookieJar responsePost1
        requestPost2 =
          (accountsPostRequest "/SmsAuth") {
            queryString = BS8.pack $ "?continue=" ++ listingUrl ++ "&service=bookmarks"
          , requestBody = RequestBodyBS $ BS8.pack $
            "continue=" ++ listingUrl
            ++ "&service=bookmarks"
            ++ "&exp=smsauthnojs"
            ++ "&smsUserPin=" ++ encode smsToken
            ++ "&PersistentCookie=yes"
          , cookieJar = Just cookieJarPost1
          , redirectCount = 0
          , checkStatus = \_ _ _ -> Nothing
          }
      responsePost2 <- httpLbs requestPost2 manager
      let
        bodyPost2 = responseXml responsePost2
        cookieJarPost2 = responseCookieJar responsePost2
        requestPost3 =
          (accountsPostRequest "/ServiceLoginAuth") {
            queryString = BS8.pack $ "?continue=" ++ listingUrl
          , requestBody = RequestBodyBS $ BS8.pack $
            "continue=" ++ listingUrl
            ++ "&smsToken=" ++ extractValue "smsToken" bodyPost2
            ++ "&GALX=" ++ extractValue "GALX" bodyGet1
            ++ "&bgresponse=js_disabled"
          , cookieJar = Just cookieJarPost2
          }
      responsePost3 <- httpLbs requestPost3 manager
      return $ responseXml responsePost3


accountsPostRequest :: String -> Request m
accountsPostRequest path =
  appendHeaders [("Content-Type", "application/x-www-form-urlencoded")] $
  def {
    method = BS8.pack "POST"
  , secure = True
  , host = BS8.pack "accounts.google.com"
  , port = 443
  , path = BS8.pack path
  }


listingUrl :: String
listingUrl = "https%3A%2F%2Fwww.google.com%2Fbookmarks%2F%3Foutput%3Dxml%26num%3D100000"


extractValue :: String -> Element -> String
extractValue value root =
  fromJust $ do
    let
      inputElement = blank_name {qName = "input"}
      nameAttribute = blank_name {qName = "name"}
      valueAttribute = blank_name {qName = "value"}
      filter element = elName element == inputElement && findAttr nameAttribute element == Just value
    element <- filterElement filter root
    findAttr valueAttribute element
