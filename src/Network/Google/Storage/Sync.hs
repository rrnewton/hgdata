-----------------------------------------------------------------------------
--
-- Module      :  Network.Google.Storage.Sync
-- Copyright   :  (c) 2012-13 Brian W Bush
-- License     :  MIT
--
-- Maintainer  :  Brian W Bush <b.w.bush@acm.org>
-- Stability   :  Stable
-- Portability :  POSIX
--
-- | Synchronization of filesystem directories with Google Storage buckets.
--
-----------------------------------------------------------------------------


{-# LANGUAGE BangPatterns #-}


module Network.Google.Storage.Sync (
  RegexExclusion
, sync
) where


import Control.Exception (SomeException, finally, handle)
import Control.Monad (filterM, liftM)
import Crypto.GnuPG (Recipient)
import Crypto.MD5 (MD5Info, md5Base64)
import qualified Data.ByteString.Lazy as LBS (ByteString, readFile)
import qualified Data.Digest.Pure.MD5 as MD5 (md5)
import Data.List ((\\), deleteFirstsBy, intersectBy)
import Data.Maybe (catMaybes, fromJust)
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (parseTime)
import Network.Google (AccessToken, ProjectId, toAccessToken)
import Network.Google.OAuth2 (OAuth2Client(..), OAuth2Tokens(..), refreshTokens, validateTokens)
import Network.Google.Storage (BucketName, KeyName, MIMEType, StorageAcl, deleteObjectUsingManager, getBucketUsingManager, putObjectUsingManager)
import Network.Google.Storage.Encrypted (putEncryptedObject, putEncryptedObjectUsingManager)
import Network.HTTP.Conduit (closeManager, def, newManager)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath (pathSeparator)
import System.IO (hFlush, stdout)
import System.Locale (defaultTimeLocale)
import System.Posix.Files (fileSize, getFileStatus, modificationTime)
import Text.Regex.Posix ((=~))
import Text.XML.Light (Element, QName(qName), filterChildrenName, filterChildName, ppTopElement, strContent)


-- | A regular expression used for excluding files from synchronization.
type RegexExclusion = String


-- | Synchronize a filesystem directory with a Google Storage bucket.
sync ::
     ProjectId         -- ^ The Google project ID.
  -> StorageAcl        -- ^ The pre-defined access control.
  -> BucketName        -- ^ The bucket name.
  -> OAuth2Client      -- ^ The OAuth 2.0 client information.
  -> OAuth2Tokens      -- ^ The OAuth 2.0 tokens.
  -> FilePath          -- ^ The directory to be synchronized.
  -> [Recipient]       -- ^ The recipients for GnuPG encryption of the uploaded files.
  -> [RegexExclusion]  -- ^ The regular expressions used for excluding files from synchronization.
  -> Bool              -- ^ Whether to write a file \".md5sum\" of MD5 sums of synchronized files into the root directory.
  -> Bool              -- ^ Whether to delete keys from the bucket that do not correspond to files on the filesystem.
  -> IO ()             -- ^ The IO action for the synchronization.
sync projectId acl bucket client tokens directory recipients exclusions md5sums purge =
  do
    manager <- newManager def
    putStrLn $ "DIRECTORY " ++ directory
    putStrLn $ "PROJECT " ++ projectId
    putStrLn $ "BUCKET " ++ bucket
    putStrLn $ "ACCESS " ++ show acl
    finally
      (
        sync'
          (getBucketUsingManager manager projectId bucket)
          ((if null recipients then putObjectUsingManager manager else putEncryptedObjectUsingManager manager recipients) projectId acl bucket)
          (deleteObjectUsingManager manager projectId bucket)
          client tokens directory
          (null recipients)
          (makeExcluder exclusions)
          md5sums
          purge
      )(
        closeManager manager
      )


-- | A function for listing a bucket.
type Lister =
     AccessToken  -- ^ The OAuth 2.0 access token.
  -> IO Element   -- ^ The action returning the XML with the metadata for the objects.


-- | A function for putting an object into a bucket.
type Putter =
     KeyName                -- ^ The object's key.
  -> Maybe MIMEType         -- ^ The object's MIME type.
  -> LBS.ByteString         -- ^ The object's data.
  -> Maybe MD5Info          -- ^ The MD5 checksum.
  -> AccessToken            -- ^ The OAuth 2.0 access token.
  -> IO [(String, String)]  -- ^ The action to put the object and return the response header.


-- | A function for deleting an object from a bucket.
type Deleter =
     KeyName                -- ^ The object's key.
  -> AccessToken            -- ^ The OAuth 2.0 access token.
  -> IO [(String, String)]  -- ^ The action to put the object and return the response header.


-- | A function for determining whether to exclude an object from synchronization.
type Excluder =
     ObjectMetadata  -- ^ The object's metadata.
  -> Bool            -- ^ Whether to exclude the object from synchronization.


-- | An expiration time and the tokens which expire then.
type TokenClock = (UTCTime, OAuth2Tokens)


-- | Check whether a token has expired and refresh it if necessary.
checkExpiration ::
     OAuth2Client   -- ^ The OAuth 2.0 client information.
  -> TokenClock     -- ^ The token and its expiration.
  -> IO TokenClock  -- ^ The action to update the token and its expiration.
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


-- | Make a function to exclude objects based on regular expressions for filenames.
makeExcluder ::
     [RegexExclusion] -- ^ The regular expressions.
  -> Excluder         -- ^ The function for excluding objects.
makeExcluder exclusions =
  \(ObjectMetadata candidate _ _ _) ->
    let
      match :: String -> Bool
      match exclusion = candidate =~ exclusion
    in
      not $ or $ map match exclusions


-- | Synchronize a filesystem directory with a Google Storage bucket.
sync' ::
     Lister        -- ^ The bucket listing function.
  -> Putter        -- ^ The object putting function.
  -> Deleter       -- ^ The object deletion function.
  -> OAuth2Client  -- ^ The OAuth 2.0 client information.
  -> OAuth2Tokens  -- ^ The OAuth 2.0 tokens.
  -> FilePath      -- ^ The directory to be synchronized.
  -> Bool          -- ^ Whether to use ETags in comparing object metadata.
  -> Excluder      -- ^ The function for excluding objects.
  -> Bool          -- ^ Whether to write a file \".md5sum\" of MD5 sums of synchronized files into the root directory.
  -> Bool          -- ^ Whether to delete keys from the bucket that do not correspond to files on the filesystem.
  -> IO ()         -- ^ The IO action for the synchronization.
sync' lister putter deleter client tokens directory byETag excluder md5sums purge =
  do
    putStr "LOCAL "
    hFlush stdout
    local <- walkDirectories directory
    putStrLn $ show $ length local
    now <- getCurrentTime
    tokenClock@(_, tokens') <- checkExpiration client (addUTCTime (-60) now, tokens)
    putStr "REMOTE "
    hFlush stdout
    remote' <- lister $ toAccessToken $ accessToken tokens'
    let
      remote = parseMetadata remote'
    putStrLn $ show $ length remote
    let
      tolerance = 300
      sameKey :: ObjectMetadata -> ObjectMetadata -> Bool
      sameKey (ObjectMetadata key _ _ _) (ObjectMetadata key' _ _ _) = key == key'
      sameETag :: ObjectMetadata -> ObjectMetadata -> Bool
      sameETag (ObjectMetadata key eTag _ _) (ObjectMetadata key' eTag' _ _) = key == key' && fst eTag == fst eTag'
      earlierTime :: ObjectMetadata -> ObjectMetadata -> Bool
      earlierTime (ObjectMetadata key _ _ time) (ObjectMetadata key' eTag' _ time') = key == key' && time > (addUTCTime tolerance time')
    putStr $ "EXCLUDED "
    hFlush stdout
    let
      local' = filter excluder local
    putStrLn $ show (length local - length local')
    putStr $ "PUTS "
    hFlush stdout
    let
      changedObjects = deleteFirstsBy (if byETag then sameETag else earlierTime) local' remote
    putStrLn $ show (length changedObjects)
    putStr $ "DELETES "
    hFlush stdout
    let
      deletedObjects = deleteFirstsBy sameKey remote local'
    putStrLn $ show (length deletedObjects)
    tokenClock' <- walkPutter client tokenClock directory putter changedObjects
    tokenClock'' <- if purge
      then
        walkDeleter client tokenClock' deleter deletedObjects
      else
        return tokenClock'
    if md5sums
      then writeFile (directory ++ "/.md5sum") $ unlines $ map (\x -> (fst . eTag) x ++ "  ./" ++ key x) local'
      else return ()


-- | Put a list of objects.
walkPutter ::
     OAuth2Client      -- ^ The OAuth 2.0 client information.
  -> TokenClock        -- ^ The token and its expiration.
  -> FilePath          -- ^ The directory to be synchronized.
  -> Putter            -- ^ The object putting function.
  -> [ObjectMetadata]  -- ^ Description of the objects to be put.
  -> IO TokenClock     -- ^ The action to update the token and its expiration.
walkPutter _ tokenClock _ _ [] = return tokenClock
walkPutter client tokenClock directory putter (x : xs) =
  do
    let
      key' = key x
    tokenClock'@(_, tokens') <- checkExpiration client tokenClock
    putStrLn $ "PUT " ++ key'
    handle
      handler
      (
        do
          bytes <- LBS.readFile $ directory ++ [pathSeparator] ++ key'
          putter key' Nothing bytes (Just $ eTag x) (toAccessToken $ accessToken tokens')
          return ()
      )
    walkPutter client tokenClock' directory putter xs


-- | Delete a list of objects.
walkDeleter ::
     OAuth2Client      -- ^ The OAuth 2.0 client information.
  -> TokenClock        -- ^ The token and its expiration.
  -> Deleter           -- ^ The object deletion function.
  -> [ObjectMetadata]  -- ^ Description of the objects to be deleted.
  -> IO TokenClock     -- ^ The action to update the token and its expiration.
walkDeleter _ tokenClock _ [] = return tokenClock
walkDeleter client tokenClock deleter (x : xs) =
  do
    let
      key' = key x
    tokenClock'@(_, tokens') <- checkExpiration client tokenClock
    putStrLn $ "DELETE " ++ key'
    handle
      handler
      (
        do
          deleter key' (toAccessToken $ accessToken tokens')
          return ()
      )
    walkDeleter client tokenClock' deleter xs


-- |
handler :: SomeException -> IO ()
handler exception = putStrLn $ "  FAIL " ++ show exception


-- | Object metadata.
data ObjectMetadata = ObjectMetadata
  {
    key :: String            -- ^ The object's key.
  , eTag :: MD5Info          -- ^ The object's MD5 sum.
  , size :: Int              -- ^ The object's size, in bytes.
  , lastModified :: UTCTime  -- ^ The object's modification time.
  }
    deriving (Show)


-- | Parse XML metadata into object descriptions.
parseMetadata ::
     Element           -- ^ The XML metadata.
  -> [ObjectMetadata]  -- ^ The object descriptions.
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
        return $ ObjectMetadata key (tail . init $ eTag, undefined) (read size) (fromJust $ parseTime defaultTimeLocale "%FT%T%QZ" lastModified)
  in
    catMaybes $ map makeMetadata $ filterChildrenName (("Contents" ==) . qName) root


-- | Gather file metadata from the file system.
walkDirectories ::
     FilePath             -- ^ The directory to be synchronized.
  -> IO [ObjectMetadata]  -- ^ Action returning file descriptions.
walkDirectories directory = walkDirectories' (directory ++ [pathSeparator]) [""]


-- | Gather file metadata from the file system.
walkDirectories' ::
     FilePath             -- ^ The directory to be synchronized.
  -> [FilePath]           -- ^ The subdirectories still remaining to be described.
  -> IO [ObjectMetadata]  -- ^ Action returning file descriptions.
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
              let !eTag = md5Base64 bytes
              let !x = fst eTag
              let !y = snd eTag
              return $ ObjectMetadata key eTag size lastTime
        files <- liftM (map ((y ++ [pathSeparator]) ++))
               $ liftM ( \\ [".", ".."])
               $ getDirectoryContents (directory ++ y)
        y' <- filterM (doesDirectoryExist . (directory ++)) files
        x' <- mapM makeMetadata $ files \\ y'
        liftM (x' ++) $ walkDirectories' directory (y' ++ ys)
    )
