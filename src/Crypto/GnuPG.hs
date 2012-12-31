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
-- | Interface to GnuPG.  The GnuPG program \"gpg\" must be on the PATH.
--
-----------------------------------------------------------------------------


module Crypto.GnuPG (
-- * Types
  Recipient
-- * Functions
, decrypt
, decryptLbs
, encrypt
, encryptLbs
) where


import Control.Concurrent (forkIO)
import qualified Data.ByteString.Lazy as LBS (ByteString, hGetContents, hPutStr)
import System.Process (runInteractiveProcess)
import System.IO (hClose, hFlush, hGetContents, hPutStr)


-- | A recipient for encryption.
type Recipient = String


-- | Decrypt text.
decrypt ::
     String     -- ^ The encrypted text.
  -> IO String  -- ^ The plain text.
decrypt input =
  do
    (hIn, hOut, _, _) <- runInteractiveProcess
      "gpg"
      [
        "--decrypt"
      , "--no-mdc-warning"
      , "--quiet"
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


-- | Encrypt text.
encrypt ::
     [Recipient]  -- ^ The recipients for encryption.
  -> String       -- ^ The plain text.
  -> IO String    -- ^ The encrypted text.
encrypt recipients input =
  do
    (hIn, hOut, _, _) <- runInteractiveProcess
      "gpg"
      (
        [
          "--encrypt"
        , "--armor"
        , "--quiet"
        , "--batch"
        ]
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


-- | Decrypt binary data.
decryptLbs ::
     LBS.ByteString     -- ^ The encrypted data.
  -> IO LBS.ByteString  -- ^ The plain data.
decryptLbs input =
  do
    (hIn, hOut, _, _) <- runInteractiveProcess
      "gpg"
      [
        "--decrypt"
      , "--no-mdc-warning"
      , "--quiet"
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


-- | Encrypt binary data.
encryptLbs ::
     [Recipient]        -- ^ The recipients for encryption.
  -> LBS.ByteString     -- ^ The plain data.
  -> IO LBS.ByteString  -- ^ The encrypted data.
encryptLbs recipients input =
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
