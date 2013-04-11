{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
-----------------------------------------------------------------------------
--
-- Module      :  Network.Google.FusionTables
-- Copyright   :  (c) 2013 Ryan Newton
-- License     :  MIT
--
-- Maintainer  :  Ryan Newton <ryan.newton@alum.mit.edu>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Functions for accessing the Fusion Tables API, see
--   <https://developers.google.com/fusiontables/>.
--
--   This provides a very limited subset of the complete (v1) API at present.
-----------------------------------------------------------------------------


module Network.Google.FusionTables (
  -- * Types
    TableId, TableMetadata(..), ColumnMetadata(..)
    
  -- * Raw API routines, returning raw JSON
  , listTables, listColumns
--  , sqlQuery

  -- * Parsing result of raw API routines
  , parseTables, parseColumns
    
  -- * Higher level interface to common SQL queries    
  , insertRows
    -- , filterRows  
) where

import           Control.Monad (liftM)
import           Data.Maybe (mapMaybe)
import           Data.List as L
import qualified Data.ByteString.Char8 as B
import           Network.Google (AccessToken, ProjectId, doRequest, makeRequest)
import           Network.HTTP.Conduit (Request(..))
import qualified Network.HTTP as H
import           Text.XML.Light (Element(elContent), QName(..), filterChildrenName, findChild, strContent)

-- TODO: Ideally this dependency wouldn't exist here and the user could select their
-- own JSON parsing lib (e.g. Aeson).
import           Text.JSON (JSObject(..), JSValue(..), Result(Ok,Error), decode, valFromObj)

-- For easy pretty printing:
import           Text.PrettyPrint.GenericPretty (Out(doc,docPrec), Generic)
import           Text.PrettyPrint.HughesPJ (text)

--------------------------------------------------------------------------------
-- Haskell Types corresponding to JSON responses

-- This could include non-ASCII characters:
-- TODO: Use Data.Text
-- type FTString = B.ByteString
type FTString = String

-- | An incomplete representation of <https://developers.google.com/fusiontables/docs/v1/reference/table#resource>
data TableMetadata =
  TableMetadata
  { tab_name    :: FTString
  , tab_tableId :: FTString
  , tab_columns :: [ColumnMetadata]
  } deriving (Eq, Show, Read, Ord, Generic)

data ColumnMetadata =
  ColumnMetadata
  { col_columnId :: Int
  , col_name :: FTString
  , col_type :: FTString
  } deriving (Eq, Show, Read, Ord, Generic)

-- TODO: Use Data.Aeson.TH

instance Out TableMetadata
instance Out ColumnMetadata
-- instance Out B.ByteString where docPrec _ = text . B.unpack; doc = docPrec 0 

-- | ID for a specific fusion table 
type TableId = FTString

--------------------------------------------------------------------------------

-- | The host for API access.
fusiontableHost :: String
-- fusiontableHost = "https://www.googleapis.com/fusiontables/v1"
fusiontableHost = "www.googleapis.com"

-- | The API version used here.
fusiontableApi :: (String, String)
fusiontableApi = ("Gdata-version", "2")
-- RRN: Is there documentation for what this means?  It seems like "Gdata" might be
-- deprecated with the new google APIs?


-- | List all tables belonging to a user.
--   See <https://developers.google.com/fusiontables/docs/v1/reference/table/list>.
listTables :: AccessToken -- ^ The OAuth 2.0 access token.
           -> IO JSValue
listTables accessToken = doRequest req
 where
   req = makeRequest accessToken fusiontableApi "GET"
                     ( fusiontableHost, "fusiontables/v1/tables" )


-- | Construct a simple Haskell representation of the result of `listTables`.
parseTables :: JSValue -> Result [TableMetadata]
parseTables (JSObject ob) = do
  JSArray allTables <- valFromObj "items" ob
  mapM parseTab allTables
 where
   parseTab :: JSValue -> Result TableMetadata
   parseTab (JSObject ob) = do
     tab_name     <- valFromObj "name"     ob
     tab_tableId  <- valFromObj "tableId" ob
     tab_columns  <- mapM parseColumn =<< valFromObj "columns" ob
     return TableMetadata {tab_name, tab_tableId, tab_columns}
   parseTab oth = Error$ "parseTable: Expected JSObject, got "++show oth
   
parseColumn :: JSValue -> Result ColumnMetadata
parseColumn (JSObject ob) = do
  col_name     <- valFromObj "name"     ob
  col_columnId <- valFromObj "columnId" ob
  col_type     <- valFromObj "type"     ob
  return ColumnMetadata {col_name, col_type, col_columnId}
parseColumn oth = Error$ "parseColumn: Expected JSObject, got "++show oth 

-- | List the columns within a specific table.
--   See <https://developers.google.com/fusiontables/docs/v1/reference/column/list>.
listColumns :: AccessToken -- ^ The OAuth 2.0 access token.
            -> TableId     -- ^ which table
            -> IO JSValue
listColumns accessToken tid = doRequest req
 where
   req = makeRequest accessToken fusiontableApi "GET"
                     ( fusiontableHost, "fusiontables/v1/tables/"++tid++"/columns" )

-- | Parse the output of `listColumns`.
parseColumns :: JSValue -> Result [ColumnMetadata]
parseColumns (JSObject ob) = do
  JSArray cols <- valFromObj "items" ob
  mapM parseColumn cols

--------------------------------------------------------------------------------

sqlQuery = error "sqlQuery"

-- | Insert one or more rows into a table.  Rows are represented as lists of strings.
-- The columns being written are passed in as a separate list.  The length of all
-- rows must match eachother and must match the list of column names.
-- 
insertRows :: AccessToken -> TableId
              -> [FTString]   -- ^ Which columns to write.
              -> [[FTString]] -- ^ Rows 
              -> IO ()
insertRows tok tid cols rows =
  do putStrLn$"DOING REQUEST "++show req
     putStrLn$ "VALS before encode "++ show vals
     doRequest req
 where
   req = (makeRequest tok fusiontableApi "GET"
           (fusiontableHost, "fusiontables/v1/query" ))
           {
             method = B.pack "POST",
             queryString = B.pack$ H.urlEncodeVars [("sql",query)]
           }
   query = concat $ L.intersperse ";\n" $
           map (("INSERT INTO "++tid++" "++ colstr ++" VALUES ")++) vals
   numcols = length cols
   colstr = parens$ concat$ L.intersperse ", " cols
   vals = map fn rows
   fn row =
     if length row == numcols
      then parens$ concat$ L.intersperse ", " $ map singQuote row
      else error$ "insertRows: got a row with an incorrect number of arguments, expected "
                  ++ show numcols ++": "++ show row

   parens s = "(" ++ s ++ ")"
   singQuote x = "'"++x++"'"



-- bulkImportRows = ...

filterRows = error "filterRows"

-- type AlbumId = String


-- -- | List the albums, see <https://developers.google.com/fusiontable-web/docs/2.0/developers_guide_protocol#ListAlbums>.
-- listAlbums ::
--      AccessToken  -- ^ The OAuth 2.0 access token.
--   -> UserId       -- ^ The user ID for the photos.
--   -> IO Element   -- ^ The action returning the albums metadata in XML format.
-- listAlbums accessToken userId =
--   doRequest $ fusiontableFeedRequest accessToken userId Nothing


-- -- | Extract the album IDs from the list of albums.
-- extractAlbumIds ::
--      Element    -- ^ The root element of the list of albums.
--   -> [AlbumId]  -- ^ The list of album IDs.
-- extractAlbumIds root =
--   let
--     idQname = QName {qName = "id", qURI = Just "http://schemas.google.com/photos/2007", qPrefix = Just "gphoto"}
--     entries :: [Element]
--     entries = filterChildrenName ((== "entry") . qName) root
--     extractAlbumId :: Element -> Maybe String
--     extractAlbumId = liftM strContent . findChild idQname
--   in
--     mapMaybe extractAlbumId entries


-- -- | List the photos in albums, see <https://developers.google.com/fusiontable-web/docs/2.0/developers_guide_protocol#ListAlbumPhotos>.
-- listPhotos ::
--      AccessToken  -- ^ The OAuth 2.0 access token.
--   -> UserId       -- ^ The user ID for the photos.
--   -> [AlbumId]    -- ^ The album ID for the photos, or all photos if null.
--   -> IO Element   -- ^ The action returning the photo metadata in XML format.
-- listPhotos accessToken userId albumIds =
--   do
--     albumIds' <-
--       if null albumIds
--         then liftM extractAlbumIds $ listAlbums accessToken userId
--         else return albumIds
--     results <- mapM (listAlbumPhotos accessToken userId) albumIds'
--     let
--       root = head results
--     return $ root {elContent = concatMap elContent results}


-- -- | List the photos in an album, see <https://developers.google.com/fusiontable-web/docs/2.0/developers_guide_protocol#ListAlbumPhotos>.
-- listAlbumPhotos ::
--      AccessToken  -- ^ The OAuth 2.0 access token.
--   -> UserId       -- ^ The user ID for the photos.
--   -> AlbumId      -- ^ The album ID for the photos.
--   -> IO Element   -- ^ The action returning the contacts in XML format.
-- listAlbumPhotos accessToken userId albumId =
--   doRequest $ fusiontableFeedRequest accessToken userId (Just albumId)


-- -- | Make an HTTP request for a Fusiontable feed.
-- fusiontableFeedRequest ::
--      AccessToken    -- ^ The OAuth 2.0 access token.
--   -> UserId         -- ^ The user ID for the photos.
--   -> Maybe AlbumId  -- ^ The album ID for the photos.
--   -> Request m      -- ^ The request.
-- fusiontableFeedRequest accessToken userId albumId =
--   makeRequest accessToken fusiontableApi "GET"
--     (
--       fusiontableHost
--     , "/data/feed/api/user/" ++ userId ++ maybe "" ("/albumid/" ++) albumId
--     )


