

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Monad (unless)
import System.Info (os)
import System.Process (system, rawSystem)
import System.Exit    (ExitCode(..))
import System.Directory (doesFileExist)
import Network.Google.OAuth2 (formUrl, exchangeCode, refreshTokens,
                              OAuth2Client(..), OAuth2Tokens(..))
import Network.Google (makeRequest, doRequest)
import Network.HTTP.Conduit (simpleHttp)

cid    = "INSTALLED_APP_CLIENT_ID"
secret = "INSTALLED_APP_SECRET_HERE"
file = "./tokens.txt"

main = do
  -- Ask for permission to read/write your fusion tables:
  let client = OAuth2Client { clientId = cid, clientSecret = secret }
      permissionUrl = formUrl client ["https://www.googleapis.com/auth/fusiontables"]

  b <- doesFileExist file
  unless b $ do 
      putStrLn$ "Load this URL: "++show permissionUrl
      case os of
        "linux"  -> rawSystem "gnome-open" [permissionUrl]
        "darwin" -> rawSystem "open"       [permissionUrl]
        _        -> return ExitSuccess
      putStrLn "Please paste the verification code: "
      authcode <- getLine
      tokens   <- exchangeCode client authcode
      putStrLn$ "Received access token: "++show (accessToken tokens)
      tokens2  <- refreshTokens client tokens
      putStrLn$ "As a test, refreshed token: "++show (accessToken tokens2)
      writeFile file (show tokens2)
  accessTok <- fmap (accessToken . read) (readFile file)
  
  putStrLn "As a test, list the users tables:"
  response <- simpleHttp ("https://www.googleapis.com/fusiontables/v1/tables?access_token="++accessTok)
  putStrLn$ unlines$ take 50$ lines$ BL.unpack response
