-----------------------------------------------------------------------------
--
-- Module      :  Crypto.MD5
-- Copyright   :  (c) 2012-13 Brian W Bush
-- License     :  MIT
--
-- Maintainer  :  Brian W Bush <b.w.bush@acm.org>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Miscellaneous functions for MD5 checksums.
--
-----------------------------------------------------------------------------


{-# LANGUAGE BangPatterns #-}


module Crypto.MD5 (
-- * Types
  MD5Info
, MD5String
, MD5Base64
, MD5Digest
-- * Functions
, md5
, md5Base64
, md5ToBase64
, md5Empty
) where


import Data.Binary as B (encode)
import Data.ByteString as BS (concat)
import Data.ByteString.Char8 as BS8 (unpack)
import Data.ByteString.Lazy as LBS (ByteString, empty, toChunks)
import Data.ByteString.Base64 as B64 (encode)
import Data.Digest.Pure.MD5 (MD5Digest, md5)


-- | MD5 checksum information.
type MD5Info = (MD5String, MD5Base64)


-- | An MD5 checksum represented as a character string.
type MD5String = String


-- | An MD5 checksum represented in base 64 encoding.
type MD5Base64 = String


-- | Compute an MD5 checksum.
md5Base64 ::
     ByteString  -- ^ The data.
  -> MD5Info     -- ^ The MD5 sum.
md5Base64 x =
  let
    !y = md5 x
    !z = md5ToBase64 y
  in
    (show y, z)


-- | Convert an MD5 digest into a base-64-encoded string.
md5ToBase64 ::
     MD5Digest  -- ^ The MD5 digest.
  -> MD5Base64  -- ^ The MD5 checksum in base 64 encoding.
md5ToBase64 = BS8.unpack . B64.encode . BS.concat . LBS.toChunks . B.encode



-- | The MD5 info for an empty string.
md5Empty :: MD5Info
md5Empty = md5Base64 empty
