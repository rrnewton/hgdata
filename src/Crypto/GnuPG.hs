-----------------------------------------------------------------------------
--
-- Module      :  Crypto.GnuPG
-- Copyright   :  (c) 2012-13 Brian W Bush
-- License     :  MIT
--
-- Maintainer  :  Brian W Bush <b.w.bush@acm.org>
-- Stability   :  Stable
-- Portability :  Linux
--
-- |
--
-----------------------------------------------------------------------------


module Crypto.GnuPG (
  decrypt
, decryptLbs
, encrypt
, encryptLbs
) where


import Control.Concurrent (forkIO)
import qualified Data.ByteString.Lazy as LBS (ByteString, hGetContents, hPutStr)
import System.Process (runInteractiveProcess)
import System.IO (hClose, hFlush, hGetContents, hPutStr)


import Data.ByteString.Util (lbsToS', sToLbs')


decrypt :: String -> IO String
decrypt input =
  do
    (hIn, hOut, _, _) <- runInteractiveProcess
      "gpg"
      [
        "--decrypt"
      , "--quiet"
      , "--no-mdc-warning"
      , "--batch"
      ]
      Nothing
      Nothing
    forkIO
      (
        do
          hPutStr hIn input
          hFlush hIn
          hClose hIn
      )
    output <- hGetContents hOut
    return output


encrypt :: Bool -> [String] -> String -> IO String
encrypt armor recipients input =
  do
    (hIn, hOut, _, _) <- runInteractiveProcess
      "gpg"
      (
        [
          "--encrypt"
        , "--quiet"
        , "--batch"
        ]
        ++
        if armor then ["--armor"] else []
        ++
        concatMap (\r -> ["--recipient", r]) recipients
      )
      Nothing
      Nothing
    forkIO
      (
        do
          hPutStr hIn input
          hFlush hIn
          hClose hIn
      )
    output <- hGetContents hOut
    return output


decryptLbs :: LBS.ByteString -> IO LBS.ByteString
decryptLbs input =
  do
    (hIn, hOut, _, _) <- runInteractiveProcess
      "gpg"
      [
        "--decrypt"
      , "--quiet"
      , "--no-mdc-warning"
      , "--batch"
      ]
      Nothing
      Nothing
    forkIO
      (
        do
          LBS.hPutStr hIn input
          hFlush hIn
          hClose hIn
      )
    output <- LBS.hGetContents hOut
    return output


encryptLbs :: Bool -> [String] -> LBS.ByteString -> IO LBS.ByteString
encryptLbs armor recipients input =
  do
    (hIn, hOut, _, _) <- runInteractiveProcess
      "gpg"
      (
        [
          "--encrypt"
        , "--quiet"
        , "--batch"
        ]
        ++
        if armor then ["--armor"] else []
        ++
        concatMap (\r -> ["--recipient", r]) recipients
      )
      Nothing
      Nothing
    forkIO
      (
        do
          LBS.hPutStr hIn input
          hFlush hIn
          hClose hIn
      )
    output <- LBS.hGetContents hOut
    return output
