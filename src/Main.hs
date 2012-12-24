{-# LANGUAGE DeriveDataTypeable #-}
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


module Main (
    main
) where


import Data.Data(Data(..))
import Network.Google (AccessToken, toAccessToken)
import Network.Google.Contacts (extractPasswords, listContacts)
import System.Console.CmdArgs


data GData =
  Contacts {
    accessToken :: String
  , xmlOutput :: FilePath
  , passwordOutput :: FilePath
  }
    deriving (Show, Data, Typeable)


gData :: GData
gData =
  modes [contacts]
    &= summary "GData v0.0.1, by B. W. Bush (b.w.bush@acm.org), CC0 1.0 Universal license."
    &= help "Process Google data.  See <http://...> for more information."


contacts :: GData
contacts = Contacts
  {
    accessToken = def &= typ "<<access token>>" &= argPos 0
  , xmlOutput = def &= typ "<<XML output file>>" &= argPos 1
  , passwordOutput = def &= typ "<<password output file>>" &= argPos 2
  }
    &= help "Download Google contacts."


main :: IO ()
main =
  do
    command <- cmdArgs gData
    dispatch command


dispatch :: GData -> IO ()
dispatch (Contacts accessToken xmlOutput passwordOutput) =
  do
    contacts <- listContacts $ toAccessToken accessToken
    writeFile xmlOutput contacts
    passwords <- extractPasswords ["example-recipient"] contacts
    writeFile passwordOutput passwords
