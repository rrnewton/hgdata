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
, sToBs
) where


import Data.ByteString as BS (ByteString, concat)
import Data.ByteString.Lazy.Char8 as LBS (ByteString, toChunks)
import Data.Text as T (pack, unpack)
import Data.Text.Encoding as T (decodeUtf8, encodeUtf8)


-- | Convert a lazy ByteString to a String, in UTF-8.
lbsToS :: LBS.ByteString -> String
lbsToS = T.unpack . T.decodeUtf8 . BS.concat . LBS.toChunks


-- | Convert a String to a ByteString, in UTF-8.
sToBs :: String -> BS.ByteString
sToBs = T.encodeUtf8 . T.pack
