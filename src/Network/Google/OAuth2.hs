-----------------------------------------------------------------------------
--
-- Module      :  Network.Google.OAuth2
-- Copyright   :  (c) 2012-13 Brian W Bush
-- License     :  MIT
--
-- Maintainer  :  Brian W Bush <b.w.bush@acm.org>
-- Stability   :  Stable
-- Portability :  Linux
--
-- |
--
-- https://developers.google.com/accounts/docs/OAuth2InstalledApp
--
-----------------------------------------------------------------------------


module Network.Google.OAuth2 (
  OAuth2Client(..)
, OAuth2Scope
, OAuth2Tokens(..)
, exchangeCode
, formUrl
, googleScopes
, refreshTokens
, validateTokens
) where


import Data.ByteString.Char8 as BS8 (ByteString, pack)
import Data.ByteString.Util (lbsToS)
import Data.CaseInsensitive as CI (CI(..), mk)
import Data.List (intercalate)
import Network.HTTP.Base (urlEncode)
import Network.HTTP.Conduit (Request(..), RequestBody(..), Response(..), def, httpLbs, responseBody, withManager)
import Text.JSON (JSObject, JSValue(JSRational), Result(Ok), decode, valFromObj)


data OAuth2Client = OAuth2Client
  {
    clientId :: String
  , clientSecret :: String
  }
    deriving (Read, Show)


type OAuth2Code = String

data OAuth2Tokens = OAuth2Tokens
  {
    accessToken :: String
  , refreshToken :: String
  , expiresIn :: Rational
  , tokenType :: String
  }
    deriving (Read, Show)


type OAuth2Scope = String


-- https://developers.google.com/oauthplayground/
googleScopes :: [(String, OAuth2Scope)]
googleScopes =
  [
    ("Adsense Management", "https://www.googleapis.com/auth/adsense")
  , ("Google Affiliate Network", "https://www.googleapis.com/auth/gan")
  , ("Analytics", "https://www.googleapis.com/auth/analytics.readonly")
  , ("Google Books", "https://www.googleapis.com/auth/books")
  , ("Blogger", "https://www.googleapis.com/auth/blogger")
  , ("Calendar", "https://www.googleapis.com/auth/calendar")
  , ("Google Cloud Storage", "https://www.googleapis.com/auth/devstorage.read_write")
  , ("Contacts", "https://www.google.com/m8/feeds/")
  , ("Content API for Shopping", "https://www.googleapis.com/auth/structuredcontent")
  , ("Chrome Web Store", "https://www.googleapis.com/auth/chromewebstore.readonly")
  , ("Documents List", "https://docs.google.com/feeds/")
  , ("Google Drive", "https://www.googleapis.com/auth/drive")
  , ("Google Drive", "Files https://www.googleapis.com/auth/drive.file")
  , ("Gmail", "https://mail.google.com/mail/feed/atom")
  , ("Google+", "https://www.googleapis.com/auth/plus.me")
  , ("Groups Provisioning", "https://apps-apis.google.com/a/feeds/groups/")
  , ("Google Latitude", "https://www.googleapis.com/auth/latitude.all.best https://www.googleapis.com/auth/latitude.all.city")
  , ("Moderator", "https://www.googleapis.com/auth/moderator")
  , ("Nicknames", "Provisioning https://apps-apis.google.com/a/feeds/alias/")
  , ("Orkut", "https://www.googleapis.com/auth/orkut")
  , ("Picasa Web", "https://picasaweb.google.com/data/")
  , ("Sites", "https://sites.google.com/feeds/")
  , ("Spreadsheets", "https://spreadsheets.google.com/feeds/")
  , ("Tasks", "https://www.googleapis.com/auth/tasks")
  , ("URL Shortener", "https://www.googleapis.com/auth/urlshortener")
  , ("Userinfo - Email", "https://www.googleapis.com/auth/userinfo.email")
  , ("Userinfo - Profile", "https://www.googleapis.com/auth/userinfo.profile")
  , ("User Provisioning", "https://apps-apis.google.com/a/feeds/user/")
  , ("Webmaster Tools", "https://www.google.com/webmasters/tools/feeds/")
  , ("YouTube", "https://gdata.youtube.com")
  ]


redirectUri :: String
redirectUri = "urn:ietf:wg:oauth:2.0:oob"


formUrl :: OAuth2Client -> [OAuth2Scope] -> String
formUrl client scopes =
  "https://accounts.google.com/o/oauth2/auth"
    ++ "?response_type=code"
    ++ "&client_id=" ++ clientId client
    ++ "&redirect_uri=" ++ redirectUri
    ++ "&scope=" ++ (intercalate "+" $ map urlEncode scopes)


exchangeCode :: OAuth2Client -> OAuth2Code -> IO OAuth2Tokens
exchangeCode client code =
  do
    result <- doOAuth2 client "authorization_code" ("&redirect_uri=" ++ redirectUri ++ "&code=" ++ code)
    let
      (Ok result') = decodeTokens result
    return result'


decodeTokens :: JSObject JSValue -> Result OAuth2Tokens
decodeTokens value =
  do
    let
      (!) = flip valFromObj
      expiresIn' :: Rational
      (Ok (JSRational _ expiresIn')) = valFromObj "expires_in" value
    accessToken <- value ! "access_token"
    refreshToken <- value ! "refresh_token"
    -- expiresIn <- value ! "expires_in"
    tokenType <- value ! "token_type"
    return OAuth2Tokens
      {
        accessToken = accessToken
      , refreshToken = refreshToken
      , expiresIn = expiresIn'
      , tokenType = tokenType
      }


refreshTokens :: OAuth2Client -> OAuth2Tokens -> IO OAuth2Tokens
refreshTokens client tokens =
  do
    result <- doOAuth2 client "refresh_token" ("&refresh_token=" ++ refreshToken tokens)
    let
      (Ok result') = decodeTokens' tokens result
    return result'


decodeTokens' :: OAuth2Tokens -> JSObject JSValue -> Result OAuth2Tokens
decodeTokens' tokens value =
  do
    let
      (!) = flip valFromObj
      expiresIn' :: Rational
      (Ok (JSRational _ expiresIn')) = valFromObj "expires_in" value
    accessToken <- value ! "access_token"
    -- expiresIn <- value ! "expires_in"
    tokenType <- value ! "token_type"
    return tokens
      {
        accessToken = accessToken
      , expiresIn = expiresIn'
      , tokenType = tokenType
      }


doOAuth2 :: OAuth2Client -> String -> String -> IO (JSObject JSValue)
doOAuth2 client grantType extraBody =
  do
    let
      makeHeaderName :: String -> CI.CI BS8.ByteString
      makeHeaderName = CI.mk . BS8.pack
      request =
        def {
          method = BS8.pack "POST"
        , secure = True
        , host = BS8.pack "accounts.google.com"
        , port = 443
        , path = BS8.pack "/o/oauth2/token"
        , requestHeaders = [
            (makeHeaderName "Content-Type",  BS8.pack "application/x-www-form-urlencoded")
          ]
        , requestBody = RequestBodyBS . BS8.pack
            $ "client_id=" ++ clientId client
            ++ "&client_secret=" ++ clientSecret client
            ++ "&grant_type=" ++ grantType
            ++ extraBody
        }
    response <- withManager $ httpLbs request
    let
      (Ok result) = decode . lbsToS $ responseBody response
    return $ result


validateTokens :: OAuth2Tokens -> IO Rational
validateTokens tokens =
  do
    let
      makeHeaderName :: String -> CI.CI BS8.ByteString
      makeHeaderName = CI.mk . BS8.pack
      request =
        def {
          method = BS8.pack "GET"
        , secure = True
        , host = BS8.pack "www.googleapis.com"
        , port = 443
        , path = BS8.pack "/oauth2/v1/tokeninfo"
        , queryString = BS8.pack ("?access_token=" ++ accessToken tokens)
        }
    response <- withManager $ httpLbs request
    let
      (Ok result) = decode . lbsToS $ responseBody response
      expiresIn' :: Rational
      (Ok (JSRational _ expiresIn')) = valFromObj "expires_in" result
    return expiresIn'
