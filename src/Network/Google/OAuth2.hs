-----------------------------------------------------------------------------
--
-- Module      :  Network.Google.OAuth2
-- Copyright   :  (c) 2012-13 Brian W Bush
-- License     :  MIT
--
-- Maintainer  :  Brian W Bush <b.w.bush@acm.org>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Functions for OAuth 2.0 authentication for Google APIs.
--
-----------------------------------------------------------------------------


module Network.Google.OAuth2 (
-- * Types
  OAuth2Client(..)
, OAuth2Scope
, OAuth2Tokens(..)
-- * Functions
, googleScopes
, formUrl
, exchangeCode
, refreshTokens
, validateTokens
) where


import Data.ByteString.Char8 as BS8 (ByteString, pack)
import Data.ByteString.Lazy.UTF8 (toString)
import Data.List (intercalate)
import Network.Google (makeHeaderName)
import Network.HTTP.Base (urlEncode)
import Network.HTTP.Conduit (Request(..), RequestBody(..), Response(..), def, httpLbs, responseBody, withManager)
import Text.JSON (JSObject, JSValue(JSRational), Result(Ok), decode, valFromObj)


-- An OAuth 2.0 client for an installed application, see <https://developers.google.com/accounts/docs/OAuth2InstalledApp>.
data OAuth2Client = OAuth2Client
  {
    clientId :: String      -- ^ The client ID.
  , clientSecret :: String  -- ^ The client secret.
  }
    deriving (Read, Show)


-- | An OAuth 2.0 code.
type OAuth2Code = String


-- | OAuth 2.0 tokens.
data OAuth2Tokens = OAuth2Tokens
  {
    accessToken :: String   -- ^ The access token.
  , refreshToken :: String  -- ^ The refresh token.
  , expiresIn :: Rational   -- ^ The number of seconds until the access token expires.
  , tokenType :: String     -- ^ The token type.
  }
    deriving (Read, Show)


-- | An OAuth 2.0 scope.
type OAuth2Scope = String


-- | The OAuth 2.0 scopes for Google APIs, see <https://developers.google.com/oauthplayground/>.
googleScopes ::
  [(String, OAuth2Scope)]  -- ^ List of names and the corresponding scopes.
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


-- | The redirect URI for an installed application, see <https://developers.google.com/accounts/docs/OAuth2InstalledApp#choosingredirecturi>.
redirectUri :: String
redirectUri = "urn:ietf:wg:oauth:2.0:oob"


-- | Form a URL for authorizing an installed application, see <https://developers.google.com/accounts/docs/OAuth2InstalledApp#formingtheurl>.
formUrl ::
     OAuth2Client   -- ^ The OAuth 2.0 client.
  -> [OAuth2Scope]  -- ^ The OAuth 2.0 scopes to be authorized.
  -> String         -- ^ The URL for authorization.
formUrl client scopes =
  "https://accounts.google.com/o/oauth2/auth"
    ++ "?response_type=code"
    ++ "&client_id=" ++ clientId client
    ++ "&redirect_uri=" ++ redirectUri
    ++ "&scope=" ++ (intercalate "+" $ map urlEncode scopes)


-- | Exchange an authorization code for tokens, see <https://developers.google.com/accounts/docs/OAuth2InstalledApp#handlingtheresponse>.
exchangeCode ::
     OAuth2Client     -- ^ The OAuth 2.0 client.
  -> OAuth2Code       -- ^ The authorization code.
  -> IO OAuth2Tokens  -- ^ The action for obtaining the tokens.
exchangeCode client code =
  do
    result <- doOAuth2 client "authorization_code" ("&redirect_uri=" ++ redirectUri ++ "&code=" ++ code)
    let
      (Ok result') = decodeTokens result
    return result'


-- | Parse OAuth 2.0 tokens.
decodeTokens ::
     JSObject JSValue     -- ^ The JSON value.
  -> Result OAuth2Tokens  -- ^ The OAuth 2.0 tokens.
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


-- | Refresh OAuth 2.0 tokens, see <https://developers.google.com/accounts/docs/OAuth2InstalledApp#refresh>.
refreshTokens ::
     OAuth2Client     -- ^ The client.
  -> OAuth2Tokens     -- ^ The tokens.
  -> IO OAuth2Tokens  -- ^ The action to refresh the tokens.
refreshTokens client tokens =
  do
    result <- doOAuth2 client "refresh_token" ("&refresh_token=" ++ refreshToken tokens)
    let
      (Ok result') = decodeTokens' tokens result
    return result'


-- | Refresh OAuth 2.0 tokens from JSON refresh data.
decodeTokens' ::
     OAuth2Tokens         -- ^ The original tokens.
  -> JSObject JSValue     -- ^ The JSON value.
  -> Result OAuth2Tokens  -- ^ The refreshed tokens.
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


-- | Peform OAuth 2.0 authentication, see <https://developers.google.com/accounts/docs/OAuth2InstalledApp#handlingtheresponse>.
doOAuth2 ::
     OAuth2Client           -- ^ The client.
  -> String                 -- ^ The grant type.
  -> String                 -- ^ The
  -> IO (JSObject JSValue)  -- ^ The action returing the JSON response from making the request.
doOAuth2 client grantType extraBody =
  do
    let
      -- TODO: In principle, we should UTF-8 encode the bytestrings packed below.
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
      (Ok result) = decode . toString $ responseBody response
    return $ result


-- | Validate OAuth 2.0 tokens, see <https://developers.google.com/accounts/docs/OAuth2Login#validatingtoken>.
validateTokens ::
     OAuth2Tokens  -- ^ The tokens.
  -> IO Rational   -- ^ The number of seconds until the access token expires.
validateTokens tokens =
  do
    let
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
      (Ok result) = decode . toString $ responseBody response
      expiresIn' :: Rational
      (Ok (JSRational _ expiresIn')) = valFromObj "expires_in" result
    return expiresIn'
