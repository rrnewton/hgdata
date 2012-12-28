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
  MD5Digest
, md5
, md5Base64
, md5ToBase64
) where


import Data.Binary as B (encode)
import Data.ByteString as BS (concat)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy (ByteString, toChunks)
import Data.ByteString.Base64 as B64 (encode)
import Data.Digest.Pure.MD5 (MD5Digest, md5)
import Numeric (readHex)

md5Base64 :: ByteString -> (String, String)
md5Base64 x =
  let
    y = md5 x
    z = md5ToBase64 y
  in
    (show y, z)


md5ToBase64 :: MD5Digest -> String
md5ToBase64 = unpack . B64.encode . BS.concat . toChunks . B.encode
