-----------------------------------------------------------------------------
--
-- Module      :  Data.ByteString.Util
-- Copyright   :  (c) 2012-13 Brian W Bush
-- License     :  MIT
--
-- Maintainer  :  Brian W Bush <b.w.bush@acm.org>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Miscellaneous functions for manipulating bytestrings.
--
-----------------------------------------------------------------------------


module Data.ByteString.Util (
  lbsToS
, lbsToS'
, sToBs
, sToLbs'
) where


import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Char8 as BS8 (concat, unpack)
import qualified Data.ByteString.Lazy.Char8 as LBS8 (ByteString, pack, toChunks)
import qualified Data.Text as T (pack, unpack)
import qualified Data.Text.Encoding as T (decodeUtf8, encodeUtf8)


-- | Convert a lazy ByteString to a String, in UTF-8.
lbsToS ::
     LBS8.ByteString -- ^ The bytestring to be converted.
  -> String          -- ^ The string corresponding to the UTF-8 decoding of the bytestring.
lbsToS = T.unpack . T.decodeUtf8 . BS8.concat . LBS8.toChunks


-- | Convert a lazy ByteString to a String, without UTF-8.
lbsToS' ::
     LBS8.ByteString -- ^ The bytestring to be converted.
  -> String          -- ^ The string corresponding to the bytestring, but with no UTF-8 decoding.
lbsToS' = BS8.unpack . BS8.concat . LBS8.toChunks


-- | Convert a String to a ByteString, in UTF-8.
sToBs ::
     String         -- ^ The string to be converted.
  -> BS.ByteString  -- ^ The bytestring corresponding to the UTF-8 encoding of the string.
sToBs = T.encodeUtf8 . T.pack


-- | Convert a String to a lazy ByteString, without UTF-8.
sToLbs' ::
     String           -- ^ The string to be converted.
  -> LBS8.ByteString  -- ^ The bytestring corresponding to the string, but with no UTF-8 encoding.
sToLbs' = LBS8.pack
