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
import Network.Google (AccessToken, toAccessToken)
import Network.Google.Contacts (extractPasswords, listContacts)
import Network.Google.Storage (deleteObject, getBucket, getObject, putObject)
import System.Console.CmdArgs
import Text.XML.Light (ppTopElement)


data GData =
    Contacts {
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
    }
  | SPut {
      accessToken :: String
    , projectId :: String
    , bucket :: String
    , key :: String
    , input :: FilePath
    , acl :: String
    }
  | SDelete {
      accessToken :: String
    , projectId :: String
    , bucket :: String
    , key :: String
    }
      deriving (Show, Data, Typeable)


gData :: GData
gData =
  modes [contacts, slist, sget, sput, sdelete]
    &= summary "GData v0.0.2, by B. W. Bush (b.w.bush@acm.org), CC0 1.0 Universal license."
    &= help "Process Google data.  See <http://...> for more information."


contacts :: GData
contacts = Contacts
  {
    accessToken = def &= typ "<<access token>>" &= argPos 0
  , xmlOutput = def &= typ "<<XML output file>>" &= argPos 1
  , passwordOutput = def &= typ "<<password output file>>" &= argPos 2
  }
    &= help "Download Google Contacts."


slist :: GData
slist = SList
  {
    accessToken = def &= typ "<<access token>>" &= argPos 0
  , projectId = def &= typ "<<project ID>>" &= argPos 1
  , bucket = def &= typ "<<bucket name>>" &= argPos 2
  , xmlOutput = def &= typ "<<XML output file>>" &= argPos 3
  }
    &= help "List objects in a Google Storage bucket."


sget :: GData
sget = SGet
  {
    accessToken = def &= typ "<<access token>>" &= argPos 0
  , projectId = def &= typ "<<project ID>>" &= argPos 1
  , bucket = def &= typ "<<bucket name>>" &= argPos 2
  , key = def &= typ "<<key name>" &= argPos 3
  , output = def &= typ "<<output file>>" &= argPos 4
  }
    &= help "Get an object from a Google Storage bucket."


sput :: GData
sput = SPut
  {
    accessToken = def &= typ "<<access token>>" &= argPos 0
  , projectId = def &= typ "<<project ID>>" &= argPos 1
  , bucket = def &= typ "<<bucket name>>" &= argPos 2
  , key = def &= typ "<<key name>" &= argPos 3
  , input = def &= typ "<<input file>>" &= argPos 4
  , acl = def &= typ "<<access control>>" &= argPos 5 &= opt "private"
  }
    &= help "Put an object into a Google Storage bucket."


sdelete :: GData
sdelete = SDelete
  {
    accessToken = def &= typ "<<access token>>" &= argPos 0
  , projectId = def &= typ "<<project ID>>" &= argPos 1
  , bucket = def &= typ "<<bucket name>>" &= argPos 2
  , key = def &= typ "<<key name>" &= argPos 3
  }
    &= help "Delete an object from a Google Storage bucket."


dispatch :: GData -> IO ()
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
dispatch (SGet accessToken projectId bucket key output) =
  do
    result <- getObject projectId (toAccessToken accessToken) bucket key
    LBS.writeFile output result
dispatch (SPut accessToken projectId bucket key input acl) =
  do
    bytes <- LBS.readFile input
    putObject projectId (toAccessToken accessToken) bucket key (read acl) Nothing bytes
    return ()
dispatch (SDelete accessToken projectId bucket key) =
  do
    result <- deleteObject projectId (toAccessToken accessToken) bucket key
    return ()


main :: IO ()
main =
  do
    command <- cmdArgs gData
    dispatch command
