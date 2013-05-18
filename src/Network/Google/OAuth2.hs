-----------------------------------------------------------------------------
--
-- Module      :  Network.Google.OAuth2
-- Copyright   :  (c) 2012-13 Brian W Bush, Ryan Newton
-- License     :  MIT
--
-- Maintainer  :  Brian W Bush <b.w.bush@acm.org>, Ryan Newton <rrnewton@indiana.edu>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Functions for OAuth 2.0 authentication for Google APIs.
--
-- If you are new to Google web API's, bear in mind that there are /three/ different
-- methods for accessing APIs (installed applications, web apps, service-to-service),
-- and this library is most useful for \"installed applications\".
--
-- Installed applications need the user to grant permission in a browser at least
-- once (see `formUrl`).  However, while the resulting `accessToken` expires quickly,
-- the `refreshToken` can be used indefinitely for retrieving new access tokens.
-- Thus this approach can be suitable for long running or periodic programs that
-- access Google data.
--
-- Below is a quick-start program which will list any Google Fusion tables the user
-- possesses.  It requires the client ID and secret retrieved from 
-- <https://code.google.com/apis/console>.
--
-- @
-- import Control.Monad (unless)
-- import System.Info (os)
-- import System.Process (system, rawSystem)
-- import System.Exit    (ExitCode(..))
-- import System.Directory (doesFileExist)
-- import Network.Google.OAuth2 (formUrl, exchangeCode, refreshTokens,
--                               OAuth2Client(..), OAuth2Tokens(..))
-- import Network.Google (makeRequest, doRequest)
-- import Network.HTTP.Conduit (simpleHttp)
-- --
-- cid    = \"INSTALLED_APP_CLIENT_ID\"
-- secret = \"INSTALLED_APP_SECRET_HERE\"
-- file   = \"./tokens.txt\"
-- --  
-- main = do
--   -- Ask for permission to read/write your fusion tables:
--   let client = OAuth2Client { clientId = cid, clientSecret = secret }
--       permissionUrl = formUrl client [\"https://www.googleapis.com/auth/fusiontables\"]
--   b <- doesFileExist file
--   unless b $ do 
--       putStrLn$ \"Load this URL: \"++show permissionUrl
--       case os of
--         \"linux\"  -> rawSystem \"gnome-open\" [permissionUrl]
--         \"darwin\" -> rawSystem \"open\"       [permissionUrl]
--         _        -> return ExitSuccess
--       putStrLn \"Please paste the verification code: \"
--       authcode <- getLine
--       tokens   <- exchangeCode client authcode
--       putStrLn$ \"Received access token: \"++show (accessToken tokens)
--       tokens2  <- refreshTokens client tokens
--       putStrLn$ \"As a test, refreshed token: \"++show (accessToken tokens2)
--       writeFile file (show tokens2)
--   accessTok <- fmap (accessToken . read) (readFile file)
--   putStrLn \"As a test, list the users tables:\"
--   response <- simpleHttp (\"https://www.googleapis.com/fusiontables/v1/tables?access_token=\"++accessTok)
--   putStrLn$ BL.unpack response
-- @

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
, getCachedTokens
) where


import Control.Monad  (unless)
import Data.ByteString.Char8 as BS8 (ByteString, pack)
import Data.ByteString.Lazy.UTF8 (toString)
import Data.List (intercalate)
import Data.Time.Clock       (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Word (Word64)
import Network.Google (makeHeaderName)
import Network.HTTP.Base (urlEncode)
import Network.HTTP.Conduit (Request(..), RequestBody(..), Response(..), def, httpLbs, responseBody, withManager)
import Text.JSON (JSObject, JSValue(JSRational), Result(Ok), decode, valFromObj)
import System.Info    (os)
import System.Process (rawSystem)
import System.Exit    (ExitCode(..))
import System.Directory (doesFileExist, doesDirectoryExist, getAppUserDataDirectory,
                         createDirectory, renameFile, removeFile)
import System.FilePath ((</>),(<.>), splitExtension)
import System.Random (randomIO)


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
  , ("Fusion Tables", "https://www.googleapis.com/auth/fusiontables")
  , ("Documents List", "https://docs.google.com/feeds/")
  , ("Google Drive", "https://www.googleapis.com/auth/drive")
  , ("Google Drive Files", "Files https://www.googleapis.com/auth/drive.file")
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
    ++ "&scope=" ++ intercalate "+" (map urlEncode scopes)


-- | Exchange an authorization code for tokens, see <https://developers.google.com/accounts/docs/OAuth2InstalledApp#handlingtheresponse>.
exchangeCode ::
     OAuth2Client     -- ^ The OAuth 2.0 client.
  -> OAuth2Code       -- ^ The authorization code.
  -> IO OAuth2Tokens  -- ^ The action for obtaining the tokens.
exchangeCode client code =
  do
    result <- doOAuth2 client "authorization_code" ("&redirect_uri=" ++ redirectUri ++ "&code=" ++ code)
    let
      (Ok result') = decodeTokens Nothing result
    return result'


-- | Refresh OAuth 2.0 tokens from JSON data.
decodeTokens ::
     Maybe OAuth2Tokens   -- ^ The original tokens, if any.
  -> JSObject JSValue     -- ^ The JSON value.
  -> Result OAuth2Tokens  -- ^ The refreshed tokens.
decodeTokens tokens value =
  do
    let
      (!) = flip valFromObj
      -- TODO: There should be a more straightforward way to do this.
      expiresIn' :: Rational
      (Ok (JSRational _ expiresIn')) = valFromObj "expires_in" value
    accessToken <- value ! "access_token"
    refreshToken <- maybe (value ! "refresh_token") (Ok . refreshToken) tokens
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
      (Ok result') = decodeTokens (Just tokens) result
    return result'


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
    return result


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


-- | Provide a hassle-free way to retrieve and refresh tokens from a users home
-- directory, OR ask the user for permission.
-- 
-- The first time it is called, this may open a web-browser, and/or request the user
-- enter data on the command line.  Subsequently, invocations on the same machine
-- should not communicate with the user.
--
-- If the tokens do not expire until more than 15 minutes in the future, this
-- procedure will skip the refresh step.  Whether or not it refreshes should be
-- immaterial to the clients subsequent actions, because all clients should handle
-- authentication errors (and all 5xx errors) and call `refreshToken` as necessary.
getCachedTokens :: OAuth2Client -- ^ The client is the \"key\" for token lookup.
                -> IO OAuth2Tokens 
getCachedTokens client = do 
   cabalD <- getAppUserDataDirectory "cabal"
   let tokenD = cabalD </> "googleAuthTokens"
       tokenF = tokenD </> clientId client <.> "token"
   d1       <- doesDirectoryExist cabalD     
   unless d1 $ createDirectory cabalD -- Race.
   d2       <- doesDirectoryExist tokenD 
   unless d2 $ createDirectory tokenD -- Race.
   f1       <- doesFileExist tokenF
   if f1 then do
      str <- readFile tokenF
      case reads str of
        -- Here's our approach to versioning!  If we can't read it, we remove it.
        ((oldtime,toks),_):_ -> do
          tagged <- checkExpiry tokenF (oldtime,toks)
          return (snd tagged)
        [] -> do
          putStrLn$" [getCachedTokens] Could not read tokens from file: "++ tokenF
          putStrLn$" [getCachedTokens] Removing tokens and re-authenticating..."
          removeFile tokenF 
          getCachedTokens client
    else do 
     toks <- askUser
     fmap snd$ timeStampAndWrite tokenF toks
 where   
   -- Tokens store a relative time, which is rather silly (relative to what?).  This
   -- routine tags a token with the time it was issued, so as to enable figuring out
   -- the absolute expiration time.  Also, as a side effect, this is where we refresh
   -- the token if it is already expired or expiring soon.
   checkExpiry :: FilePath -> (Rational, OAuth2Tokens) -> IO (Rational, OAuth2Tokens)
   checkExpiry tokenF orig@(start1,toks1) = do
     t <- getCurrentTime 
     let nowsecs = toRational (utcTimeToPOSIXSeconds t)
         expire1 = start1 + expiresIn toks1
         tolerance = 15 * 60 -- Skip refresh if token is good for at least 15 min.
     if (expire1 < tolerance + nowsecs) then do
       toks2 <- refreshTokens client toks1
       timeStampAndWrite tokenF toks2
      else return orig

   timeStampAndWrite :: FilePath -> OAuth2Tokens -> IO (Rational, OAuth2Tokens)
   timeStampAndWrite tokenF toks = do 
       t2 <- getCurrentTime       
       let tagged = (toRational (utcTimeToPOSIXSeconds t2), toks)
       atomicWriteFile tokenF (show tagged)
       return tagged

   -- This is the part where we require user interaction:
   askUser = do 
     putStrLn$ " [getCachedTokens] Load this URL: "++show permissionUrl
     runBrowser 
     putStrLn " [getCachedTokens] Then please paste the verification code and press enter:\n$ "
     authcode <- getLine
     tokens   <- exchangeCode client authcode
     putStrLn$ " [getCachedTokens] Received access token: "++show (accessToken tokens)
     return tokens

   permissionUrl = formUrl client ["https://www.googleapis.com/auth/fusiontables"]

   -- This is hackish and incomplete 
   runBrowser =
      case os of
        "linux"  -> rawSystem "gnome-open" [permissionUrl]
        "darwin" -> rawSystem "open"       [permissionUrl]
        _        -> return ExitSuccess

   atomicWriteFile file str = do 
     suff <- randomIO :: IO Word64
     let (root,ext) = splitExtension file
         tmp = root ++ show suff <.> ext     
     writeFile tmp str
     -- RenameFile makes this atomic:
     renameFile tmp file

