-----------------------------------------------------------------------------
--
-- Module      :  Network.Google.Storage.Sync
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


{-# LANGUAGE BangPatterns #-}


module Network.Google.Storage.Sync (
  sync
) where


import Control.Exception (SomeException, finally, handle)
import Control.Monad (filterM, liftM)
import qualified Data.ByteString.Lazy as LBS(ByteString, readFile)
import qualified Data.Digest.Pure.MD5 as MD5 (md5)
import Data.List ((\\), deleteFirstsBy, intersectBy)
import Data.Maybe (catMaybes, fromJust)
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (parseTime)
import Network.Google (AccessToken, toAccessToken)
import Network.Google.OAuth2 (OAuth2Client(..), OAuth2Tokens(..), refreshTokens, validateTokens)
import Network.Google.Storage (StorageAcl, deleteObjectUsingManager, getBucketUsingManager, putObjectUsingManager)
import Network.Google.Storage.Encrypted (putEncryptedObject, putEncryptedObjectUsingManager)
import Network.HTTP.Conduit (closeManager, def, newManager)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath (pathSeparator)
import System.IO (hFlush, stdout)
import System.Locale (defaultTimeLocale)
import System.Posix.Files (fileSize, getFileStatus, modificationTime)
import Text.Regex.Posix ((=~))
import Text.XML.Light (Element, QName(qName), filterChildrenName, filterChildName, ppTopElement, strContent)


type Lister = AccessToken -> IO Element
type Putter = String -> Maybe String -> LBS.ByteString -> AccessToken -> IO [(String, String)]
type Deleter = String -> AccessToken -> IO [(String, String)]
type Excluder = ObjectMetadata -> Bool


sync :: String -> StorageAcl -> String -> OAuth2Client -> OAuth2Tokens -> FilePath -> [String] -> [String] -> IO ()
sync projectId acl bucket client tokens directory recipients exclusions =
  do
    manager <- newManager def
    finally
      (
        sync'
          (getBucketUsingManager manager projectId bucket)
          ((if null recipients then putObjectUsingManager manager else putEncryptedObjectUsingManager manager recipients) projectId acl bucket)
          (deleteObjectUsingManager manager projectId bucket)
          client tokens directory
          (null recipients)
          (makeExcluder exclusions)
      )(
        closeManager manager
      )


type TokenClock = (UTCTime, OAuth2Tokens)


checkExpiration :: OAuth2Client -> TokenClock -> IO TokenClock
checkExpiration client (expirationTime, tokens) =
  do
    now <- getCurrentTime
    if now > expirationTime
      then
        do
          tokens' <- refreshTokens client tokens
          start <- getCurrentTime
          let
            expirationTime' = addUTCTime (fromRational (expiresIn tokens') - 60) start
          putStrLn $ "REFRESH " ++ show expirationTime'
          return (expirationTime', tokens')
       else
         return (expirationTime, tokens)


makeExcluder :: [String] -> Excluder
makeExcluder exclusions =
  \(ObjectMetadata candidate _ _ _) ->
    let
      match :: String -> Bool
      match exclusion = candidate =~ exclusion
    in
      not $ or $ map match exclusions


sync' :: Lister -> Putter -> Deleter -> OAuth2Client -> OAuth2Tokens -> FilePath -> Bool -> Excluder -> IO ()
sync' lister putter deleter client tokens directory byETag excluder =
  do
    now <- getCurrentTime
    tokenClock@(_, tokens') <- checkExpiration client (addUTCTime (-60) now, tokens)
    putStr "REMOTE "
    hFlush stdout
    remote' <- lister $ toAccessToken $ accessToken tokens'
    let
      remote = parseMetadata remote'
    putStrLn $ show $ length remote
    putStr "LOCAL "
    hFlush stdout
    local <- walkDirectories directory
    putStrLn $ show $ length local
    let
      tolerance = 300
      sameKey :: ObjectMetadata -> ObjectMetadata -> Bool
      sameKey (ObjectMetadata key _ _ _) (ObjectMetadata key' _ _ _) = key == key'
      sameETag :: ObjectMetadata -> ObjectMetadata -> Bool
      sameETag (ObjectMetadata key eTag _ _) (ObjectMetadata key' eTag' _ _) = key == key' && eTag == eTag'
      earlierTime :: ObjectMetadata -> ObjectMetadata -> Bool
      earlierTime (ObjectMetadata key _ _ time) (ObjectMetadata key' eTag' _ time') = key == key' && time > (addUTCTime tolerance time')
      local' = filter excluder local
      changedObjects = deleteFirstsBy (if byETag then sameETag else earlierTime) local' remote
      deletedObjects = deleteFirstsBy sameKey remote local'
    putStrLn $ "EXCLUDED " ++ show (length local - length local')
    putStrLn $ "PUTS " ++ show (length changedObjects)
    putStrLn $ "DELETES " ++ show (length deletedObjects)
    tokenClock' <- walkPutter client tokenClock directory putter changedObjects
    tokenClock'' <- walkDeleter client tokenClock' deleter deletedObjects
    return ()


walkPutter :: OAuth2Client -> TokenClock -> FilePath -> Putter -> [ObjectMetadata] -> IO TokenClock
walkPutter _ tokenClock _ _ [] = return tokenClock
walkPutter client (expirationTime, tokens) directory putter (x : xs) =
  do
    let
      key' = key x
    (expirationTime', tokens') <- checkExpiration client (expirationTime, tokens)
    putStrLn $ "PUT " ++ key'
    handle
      handler
      (
        do
          bytes <- LBS.readFile $ directory ++ [pathSeparator] ++ key'
          putter key' Nothing bytes (toAccessToken $ accessToken tokens')
          return ()
      )
    walkPutter client (expirationTime', tokens') directory putter xs


walkDeleter :: OAuth2Client -> TokenClock -> Deleter -> [ObjectMetadata] -> IO TokenClock
walkDeleter _ tokenClock _ [] = return tokenClock
walkDeleter client (expirationTime, tokens) deleter (x : xs) =
  do
    let
      key' = key x
    (expirationTime', tokens') <- checkExpiration client (expirationTime, tokens)
    putStrLn $ "DELETE " ++ key'
    handle
      handler
      (
        do
          deleter key' (toAccessToken $ accessToken tokens')
          return ()
      )
    walkDeleter client (expirationTime, tokens') deleter xs


handler :: SomeException -> IO ()
handler exception = putStrLn $ "  FAIL " ++ show exception


data ObjectMetadata = ObjectMetadata
  {
    key :: String
  , eTag :: String
  , size :: Int
  , lastModified :: UTCTime
  }
    deriving (Show)


parseMetadata :: Element -> [ObjectMetadata]
parseMetadata root =
  let
    makeMetadata :: Element -> Maybe ObjectMetadata
    makeMetadata element =
      do
        let
          finder :: String -> Element -> Maybe String
          finder name = liftM strContent . filterChildName ((name ==) . qName)
        key <- finder "Key" element
        eTag <- finder "ETag" element
        size <- finder "Size" element
        lastModified <- finder "LastModified" element
        return $ ObjectMetadata key (tail . init $ eTag) (read size) (fromJust $ parseTime defaultTimeLocale "%FT%T%QZ" lastModified)
  in
    catMaybes $ map makeMetadata $ filterChildrenName (("Contents" ==) . qName) root


walkDirectories :: FilePath -> IO [ObjectMetadata]
walkDirectories directory = walkDirectories' (directory ++ [pathSeparator]) [""]

walkDirectories' :: FilePath -> [FilePath] -> IO [ObjectMetadata]
walkDirectories' _ [] = return []
walkDirectories' directory (y : ys) =
  handle
    ((
      \exception ->
        do
          putStrLn $ "  LIST " ++ y
          putStrLn $ "    FAIL " ++ show exception
          walkDirectories' directory ys
    ) :: SomeException -> IO [ObjectMetadata])
    (
      do
        let
          makeMetadata :: FilePath -> IO ObjectMetadata
          makeMetadata file =
            do
              let
                key = tail file
                path = directory ++ key
              bytes <- LBS.readFile path
              status <- getFileStatus path
              let
                lastTime :: UTCTime
                lastTime = posixSecondsToUTCTime $ realToFrac $ modificationTime status
                size :: Int
                size = fromIntegral $ fileSize status
              let !eTag = MD5.md5 bytes
              return $ ObjectMetadata key (show eTag) size lastTime
        files <- liftM (map ((y ++ [pathSeparator]) ++))
               $ liftM ( \\ [".", ".."])
               $ getDirectoryContents (directory ++ y)
        y' <- filterM (doesDirectoryExist . (directory ++)) files
        x' <- mapM makeMetadata $ files \\ y'
        liftM (x' ++) $ walkDirectories' directory (y' ++ ys)
    )
