-----------------------------------------------------------------------------
--
-- Module      :  Data.ByteString.Util
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


module Data.ByteString.Util (
  lbsToS
, lbsToS'
, sToBs
) where


import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Char8 as BS8 (concat, unpack)
import qualified Data.ByteString.Lazy.Char8 as LBS8 (ByteString, toChunks)
import qualified Data.Text as T (pack, unpack)
import qualified Data.Text.Encoding as T (decodeUtf8, encodeUtf8)


-- | Convert a lazy ByteString to a String, in UTF-8.
lbsToS :: LBS8.ByteString -> String
lbsToS = T.unpack . T.decodeUtf8 . BS8.concat . LBS8.toChunks


-- | Convert a lazy ByteString to a String, without UTF-8.
lbsToS' :: LBS8.ByteString -> String
lbsToS' = BS8.unpack . BS8.concat . LBS8.toChunks


-- | Convert a String to a ByteString, in UTF-8.
sToBs :: String -> BS.ByteString
sToBs = T.encodeUtf8 . T.pack
