-----------------------------------------------------------------------------
--
-- Module      :  Crypto.MD5
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


module Crypto.MD5 (
  md5Base64
) where


import Data.Binary as B (encode)
import Data.ByteString as BS (concat)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy (ByteString, toChunks)
import Data.ByteString.Base64 as B64 (encode)
import Data.Digest.Pure.MD5 (md5)


md5Base64 :: ByteString -> String
md5Base64 = unpack . B64.encode . BS.concat . toChunks . B.encode . md5
