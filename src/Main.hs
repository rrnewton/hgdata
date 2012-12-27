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
import Network.Google.Storage (StorageAcl(Private), deleteObject, getBucket, getObject, headObject, putObject)
import Network.Google.Storage.Encrypted (getEncryptedObject, putEncryptedObject)
import Network.Google.Storage.Sync (sync)
import System.Console.CmdArgs
import Text.XML.Light (ppTopElement)


data HGData =
    OAuth2Url {
      client :: String
   }
  | OAuth2Exchange {
      client :: String
    , secret :: String
    , code :: String
    , tokens :: FilePath
  }
  | OAuth2Refresh {
      client :: String
    , secret :: String
    , refresh :: String
    , tokens :: FilePath
  }
  | Contacts {
      access :: String
    , xml :: FilePath
    , passwords :: Maybe FilePath
    , encrypt :: [String]
    }
  | SList {
      access :: String
    , project :: String
    , bucket :: String
    , xml :: FilePath
    }
  | SGet {
      access :: String
    , project :: String
    , bucket :: String
    , key :: String
    , output :: FilePath
    , decrypt :: Bool
    }
  | SPut {
      access :: String
    , project :: String
    , bucket :: String
    , key :: String
    , input :: FilePath
    , acl :: String
    , encrypt :: [String]
    }
  | SDelete {
      access :: String
    , project :: String
    , bucket :: String
    , key :: String
    }
  | SHead {
      access :: String
    , project :: String
    , bucket :: String
    , key :: String
    , output :: FilePath
  }
  | SSync {
      client :: String
    , secret :: String
    , refresh :: String
    , project :: String
    , bucket :: String
    , directory :: FilePath
    , acl :: String
    , encrypt :: [String]
    , exclusions :: Maybe FilePath
    }
      deriving (Show, Data, Typeable)


hgData :: HGData
hgData =
  modes [oAuth2Url, oAuth2Exchange, oAuth2Refresh, contacts, slist, sget, sput, sdelete, shead, ssync]
    &= summary "hgdata v0.0.2, by B. W. Bush (b.w.bush@acm.org), CC0 1.0 Universal license."
    &= program "hgdata"
    &= help "Process Google data.  See <http://...> for more information."


oAuth2Url :: HGData
oAuth2Url = OAuth2Url
  {
    client = def &= typ "ID" &= help "Client ID"
  }
    &= help "Generate an OAuth2 URL."


oAuth2Exchange :: HGData
oAuth2Exchange = OAuth2Exchange
  {
    client = def &= typ "ID" &= help "OAuth2 client ID"
  , secret = def &= typ "SECRET" &= help "OAuth2 client secret"
  , code = def &= typ "CODE" &= argPos 0
  , tokens = def &= opt "/dev/stdout" &= typFile &= help "File for OAuth2 tokens"
  }
    &= help "Exchange an OAuth2 code for tokens."


oAuth2Refresh :: HGData
oAuth2Refresh = OAuth2Refresh
  {
    client = def &= typ "ID" &= help "OAuth2 client ID"
  , secret = def &= typ "SECRET" &= help "OAuth2 client secret"
  , refresh = def &= typ "TOKEN" &= help "OAuth2 refresh token"
  , tokens = def &= opt "/dev/stdout" &= typFile &= help "Output for OAuth2 tokens"
  }
    &= help "Exchange an OAuth2 code for tokens."


contacts :: HGData
contacts = Contacts
  {
    access = def &= typ "TOKEN" &= help "OAuth2 access token"
  , xml = def &= typFile &= argPos 0
  , passwords = def &= typFile &= help "Output for passwords from \"Notes\" field"
  , encrypt = def &= typ "RECIPIENT" &= help "recipient to encrypt for"
  }
    &= help "Download Google Contacts."


slist :: HGData
slist = SList
  {
    access = def &= typ "TOKEN" &= help "OAuth2 access token"
  , project = def &= typ "ID" &= help "Google API project number"
  , bucket = def &= typ "BUCKET" &= argPos 0
  , xml = def &= opt "/dev/stdout" &= typFile &= argPos 1
  }
    &= help "List objects in a Google Storage bucket."


sget :: HGData
sget = SGet
  {
    access = def &= typ "TOKEN" &= help "OAuth2 access token"
  , project = def &= typ "ID" &= help "Google API project number"
  , bucket = def &= typ "BUCKET" &= argPos 0
  , key = def &= typ "KEY" &= argPos 1
  , output = def &= opt "/dev/stdout" &= typFile &= argPos 2
  , decrypt = def &= help "Attempt to decrypt the object"
  }
    &= help "Get an object from a Google Storage bucket."


sput :: HGData
sput = SPut
  {
    access = def &= typ "TOKEN" &= help "OAuth2 access token"
  , project = def &= typ "ID" &= help "Google API project number"
  , bucket = def &= typ "BUCKET" &= argPos 0
  , key = def &= typ "KEY" &= argPos 1
  , input = def &= opt "/dev/stdin" &= typFile &= argPos 2
  , acl = def &= {-- FIXME: This doesn't seem to work. --} opt "private" &= typ "ACCESS" &= help "Pre-defined ACL"
  , encrypt = def &= typ "RECIPIENT" &= help "Recipient to encrypt for"
  }
    &= help "Put an object into a Google Storage bucket."


sdelete :: HGData
sdelete = SDelete
  {
    access = def &= typ "TOKEN" &= help "OAuth2 access token"
  , project = def &= typ "ID" &= help "Google API project number"
  , bucket = def &= typ "BUCKET" &= argPos 0
  , key = def &= typ "KEY" &= argPos 1
  }
    &= help "Delete an object from a Google Storage bucket."


shead :: HGData
shead = SHead
  {
    access = def &= typ "TOKEN" &= help "OAuth2 access token"
  , project = def &= typ "ID" &= help "Google API project number"
  , bucket = def &= typ "BUCKET" &= argPos 0
  , key = def &= typ "KEY" &= argPos 1
  , output = def &= opt "/dev/stdout" &= typFile &= argPos 2
  }
    &= help "Get object metadata from a Google Storage bucket."


ssync :: HGData
ssync = SSync
  {
    client = def &= typ "ID" &= help "OAuth2 client ID"
  , secret = def &= typ "SECRET" &= help "OAuth2 client secret"
  , refresh = def &= typ "TOKEN" &= help "OAuth2 refresh token"
  , project = def &= typ "ID" &= help "Google API project number"
  , bucket = def &= typ "BUCKET" &= argPos 0
  , directory = def &= typ "DIRECTORY" &= argPos 1
  , acl = def &= {-- FIXME: This doesn't seem to work. --} opt "private" &= typ "ACCESS" &= help "Pre-defined ACL"
  , encrypt = def &= typ "RECIPIENT" &= help "Recipient to encrypt for"
  , exclusions = def &= typFile &= help "File of regex exclusions"
  }
    &= help "Synchronize a directory with a Google Storage bucket."


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

dispatch (Contacts accessToken xmlOutput passwordOutput recipients) =
  do
    contacts <- liftM ppTopElement . listContacts $ toAccessToken accessToken
    writeFile xmlOutput contacts
    maybe
      (return ())
      (
        \x ->
        do
          passwords <- extractPasswords recipients contacts
          writeFile x passwords
      )
      passwordOutput

dispatch (SList accessToken projectId bucket xmlOutput) =
  do
    result <- getBucket projectId bucket (toAccessToken accessToken)
    writeFile xmlOutput $ ppTopElement result

dispatch (SGet accessToken projectId bucket key output decrypt) =
  do
    let getter = if decrypt then getEncryptedObject else getObject
    result <- getter projectId bucket key (toAccessToken accessToken)
    LBS.writeFile output result

dispatch (SPut accessToken projectId bucket key input acl recipients) =
  do
    let putter = if null recipients then putObject else putEncryptedObject recipients
    bytes <- LBS.readFile input
    putter projectId (read acl) bucket key Nothing bytes (toAccessToken accessToken)
    return ()

dispatch (SDelete accessToken projectId bucket key) =
  do
    result <- deleteObject projectId bucket key (toAccessToken accessToken)
    return ()

dispatch (SHead accessToken projectId bucket key output) =
  do
    result <- headObject projectId bucket key (toAccessToken accessToken)
    writeFile output $ show result

dispatch (SSync clientId clientSecret refreshToken projectId bucket directory acl recipients exclusionFile) =
  do
    let
      acl' = if acl == "" then Private else read acl
    exclusions <- liftM lines $ maybe (return "") readFile exclusionFile
    sync projectId acl' bucket (OA2.OAuth2Client clientId clientSecret) (OA2.OAuth2Tokens undefined refreshToken undefined undefined) directory recipients exclusions


main :: IO ()
main =
  do
    command <- cmdArgs hgData
    dispatch command
