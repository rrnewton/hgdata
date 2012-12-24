-----------------------------------------------------------------------------
--
-- Module      :  Main
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


{-# LANGUAGE DeriveDataTypeable #-}


module Main (
    main
) where


import Control.Monad (liftM)
import Data.Data(Data(..))
import qualified Data.ByteString.Lazy as LBS (readFile, writeFile)
import Data.Maybe (catMaybes)
import Network.Google (AccessToken, toAccessToken)
import Network.Google.Contacts (extractPasswords, listContacts)
import qualified Network.Google.OAuth2 as OA2 (OAuth2Client(..), OAuth2Tokens(..), exchangeCode, formUrl, googleScopes, refreshTokens)
import Network.Google.Storage (deleteObject, getBucket, getObject, putObject)
import Network.Google.Storage.Encrypted (getEncryptedObject, putEncryptedObject)
import System.Console.CmdArgs
import Text.XML.Light (ppTopElement)


data HGData =
    OAuth2Url {
      clientId :: String
   }
  | OAuth2Exchange {
      clientId :: String
    , clientSecret :: String
    , code :: String
    , tokenFile :: FilePath
  }
  | OAuth2Refresh {
      clientId :: String
    , clientSecret :: String
    , refreshToken :: String
    , tokenFile :: FilePath
  }
  | Contacts {
      accessToken :: String
    , xmlOutput :: FilePath
    , passwordOutput :: FilePath
    }
  | SList {
      accessToken :: String
    , projectId :: String
    , bucket :: String
    , xmlOutput :: FilePath
    }
  | SGet {
      accessToken :: String
    , projectId :: String
    , bucket :: String
    , key :: String
    , output :: FilePath
    , decrypt :: Bool
    }
  | SPut {
      accessToken :: String
    , projectId :: String
    , bucket :: String
    , key :: String
    , input :: FilePath
    , acl :: String
    , encrypt :: [String]
    }
  | SDelete {
      accessToken :: String
    , projectId :: String
    , bucket :: String
    , key :: String
    }
      deriving (Show, Data, Typeable)


hgData :: HGData
hgData =
  modes [oAuth2Url, oAuth2Exchange, oAuth2Refresh, contacts, slist, sget, sput, sdelete]
    &= summary "hGData v0.0.2, by B. W. Bush (b.w.bush@acm.org), CC0 1.0 Universal license."
    &= help "Process Google data.  See <http://...> for more information."


oAuth2Url :: HGData
oAuth2Url = OAuth2Url
  {
    clientId = def &= typ "<<client ID>>" &= argPos 0
  }
    &= help "Generate an OAuth 2 URL."


oAuth2Exchange :: HGData
oAuth2Exchange = OAuth2Exchange
  {
    clientId = def &= typ "<<client ID>>" &= argPos 0
  , clientSecret = def &= typ "<<client secret>>" &= argPos 2
  , code = def &= typ "<<exchange code>>" &= argPos 3
  , tokenFile = def &= typ "<<token file>>" &= argPos 4 &= opt "/dev/stdout"
  }
    &= help "Exchange an OAuth 2 code for tokens."


oAuth2Refresh :: HGData
oAuth2Refresh = OAuth2Refresh
  {
    clientId = def &= typ "<<client ID>>" &= argPos 0
  , clientSecret = def &= typ "<<client secret>>" &= argPos 2
  , refreshToken = def &= typ "<<refresh token>>" &= argPos 3
  , tokenFile = def &= typ "<<token file>>" &= argPos 4 &= opt "/dev/stdout"
  }
    &= help "Exchange an OAuth 2 code for tokens."


contacts :: HGData
contacts = Contacts
  {
    accessToken = def &= typ "<<access token>>" &= argPos 0
  , xmlOutput = def &= typ "<<XML output file>>" &= argPos 1
  , passwordOutput = def &= typ "<<password output file>>" &= argPos 2
  }
    &= help "Download Google Contacts."


slist :: HGData
slist = SList
  {
    accessToken = def &= typ "<<access token>>" &= argPos 0
  , projectId = def &= typ "<<project ID>>" &= argPos 1
  , bucket = def &= typ "<<bucket name>>" &= argPos 2
  , xmlOutput = def &= typ "<<XML output file>>" &= argPos 3
  }
    &= help "List objects in a Google Storage bucket."


sget :: HGData
sget = SGet
  {
    accessToken = def &= typ "<<access token>>" &= argPos 0
  , projectId = def &= typ "<<project ID>>" &= argPos 1
  , bucket = def &= typ "<<bucket name>>" &= argPos 2
  , key = def &= typ "<<key name>" &= argPos 3
  , output = def &= typ "<<output file>>" &= argPos 4
  , decrypt = def &= typ "whether to decrypt the object"
  }
    &= help "Get an object from a Google Storage bucket."


sput :: HGData
sput = SPut
  {
    accessToken = def &= typ "<<access token>>" &= argPos 0
  , projectId = def &= typ "<<project ID>>" &= argPos 1
  , bucket = def &= typ "<<bucket name>>" &= argPos 2
  , key = def &= typ "<<key name>" &= argPos 3
  , input = def &= typ "<<input file>>" &= argPos 4
  , acl = def &= typ "<<access control>>" &= argPos 5 &= opt "private"
  , encrypt = def &= typ "<<recipient for which to encrypt the object>>"
  }
    &= help "Put an object into a Google Storage bucket."


sdelete :: HGData
sdelete = SDelete
  {
    accessToken = def &= typ "<<access token>>" &= argPos 0
  , projectId = def &= typ "<<project ID>>" &= argPos 1
  , bucket = def &= typ "<<bucket name>>" &= argPos 2
  , key = def &= typ "<<key name>" &= argPos 3
  }
    &= help "Delete an object from a Google Storage bucket."


dispatch :: HGData -> IO ()
dispatch (OAuth2Url clientId) =
  do
   putStrLn $ OA2.formUrl (OA2.OAuth2Client clientId undefined) $
      catMaybes $ map (flip lookup OA2.googleScopes) ["Google Cloud Storage", "Contacts"]
dispatch (OAuth2Exchange clientId clientSecret exchangeCode tokenFile) =
  do
    tokens <- OA2.exchangeCode (OA2.OAuth2Client clientId clientSecret) exchangeCode
    writeFile tokenFile $ show tokens
dispatch (OAuth2Refresh clientId clientSecret refreshToken tokenFile) =
  do
    tokens <- OA2.refreshTokens (OA2.OAuth2Client clientId clientSecret) (OA2.OAuth2Tokens undefined refreshToken undefined undefined)
    writeFile tokenFile $ show tokens
dispatch (Contacts accessToken xmlOutput passwordOutput) =
  do
    contacts <- liftM ppTopElement . listContacts $ toAccessToken accessToken
    writeFile xmlOutput contacts
    passwords <- extractPasswords ["example-recipient"] contacts
    writeFile passwordOutput passwords
dispatch (SList accessToken projectId bucket xmlOutput) =
  do
    result <- getBucket projectId (toAccessToken accessToken) bucket
    writeFile xmlOutput $ ppTopElement result
dispatch (SGet accessToken projectId bucket key output decrypt) =
  do
    let getter = if decrypt then getEncryptedObject else getObject
    result <- getter projectId (toAccessToken accessToken) bucket key
    LBS.writeFile output result
dispatch (SPut accessToken projectId bucket key input acl recipients) =
  do
    print recipients
    let putter = if null recipients then putObject else putEncryptedObject recipients
    bytes <- LBS.readFile input
    putter projectId (toAccessToken accessToken) bucket key (read acl) Nothing bytes
    return ()
dispatch (SDelete accessToken projectId bucket key) =
  do
    result <- deleteObject projectId (toAccessToken accessToken) bucket key
    return ()


main :: IO ()
main =
  do
    command <- cmdArgs hgData
    dispatch command
