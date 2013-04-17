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
    TableId, TableMetadata(..), ColumnMetadata(..), CellType(..)
    
  -- * Raw API routines, returning raw JSON
  , createTable
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
import qualified Data.ByteString.Lazy.Char8 as BL
import           Network.Google (AccessToken, ProjectId, doRequest, makeRequest, appendBody)
import           Network.HTTP.Conduit (Request(..), RequestBody(..),parseUrl)
import qualified Network.HTTP as H
import           Text.XML.Light (Element(elContent), QName(..), filterChildrenName, findChild, strContent)

-- TODO: Ideally this dependency wouldn't exist here and the user could select their
-- own JSON parsing lib (e.g. Aeson).
import           Text.JSON (JSObject(..), JSValue(..), Result(Ok,Error),
                            decode, valFromObj, toJSObject, toJSString)
import           Text.JSON.Pretty (pp_value)

-- For easy pretty printing:
import           Text.PrettyPrint.GenericPretty (Out(doc,docPrec), Generic)
import           Text.PrettyPrint.HughesPJ (text, render)
import           Text.Printf (printf)

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
-- FIXME: This is meaningless because we're using the new discovery-based API, *NOT*
-- the old GData APIs.
fusiontableApi = ("Gdata-version", "999")

-- | Create an (exportable) table with a given name and list of columns.
createTable :: AccessToken -> String -> [(FTString,CellType)] -> IO TableId
createTable tok name cols =
  do putStrLn$ "CREATE TABLE REQUEST" ++show req

     let raw = "https://www.googleapis.com/fusiontables/v1/tables?access_token="++
               H.urlEncodeVars [("access_token", B.unpack tok)]
     putStrLn$ "RAW request: "++raw  

     initReq <- parseUrl "http://www.example.com/path"
     let req = initReq
               { method = B.pack "POST"
               , requestBody = RequestBodyBS (B.pack json)
               }

--  print =<< C.curlPost postStr []

     doRequest req
 where
   req = appendBody (BL.pack json)
         (makeRequest tok fusiontableApi "POST"
           (fusiontableHost, "fusiontables/v1/tables" ))
   json :: String
--    kind: \"fusiontables#table\"
--   json = printf "{ name: %s, isExportable: true, columns: %s }" nameJS coljson
   -- nameJS  = render$ pp_value$ JSString$ toJSString$ name
   -- coljson = render$ pp_value$ JSArray (map fn cols)

   json = render$ pp_value$ JSObject$ toJSObject$
          [ ("name",str name)
          , ("isExportable", JSBool True)
          , ("columns", colsJS) ]
   colsJS = JSArray (map fn cols)
   fn (colName, colTy) = JSObject$ 
     toJSObject [ ("name", str colName)
                , ("kind", str "fusiontables#column")  
                , ("type", str$ show colTy) ]
   str = JSString . toJSString


-- | Designed to mirror the types listed here:
--   <https://developers.google.com/fusiontables/docs/v1/reference/column>
data CellType = NUMBER | STRING | LOCATION | DATETIME
  deriving (Show,Eq,Ord,Read)

-- | List all tables belonging to a user.
--   See <https://developers.google.com/fusiontables/docs/v1/reference/table/list>.
listTables :: AccessToken -- ^ The OAuth 2.0 access token.
           -> IO JSValue
listTables accessToken = doRequest req
 where
   req  = makeRequest accessToken fusiontableApi "GET"
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
   req = (makeRequest tok fusiontableApi "POST"
           (fusiontableHost, "fusiontables/v1/query" ))
           {
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


-- | Implement a larger quantity of rows, but with the caveat that the number and order
-- of columns must exactly match the schema of the fusion table on the server.
-- `bulkImportRows` will perform a listing of the columns to verify this before uploading.
bulkImportRows :: AccessToken -> TableId
              -> [FTString]   -- ^ Which columns to write.
              -> [[FTString]] -- ^ Rows 
              -> IO ()
bulkImportRows tok tid cols rows = do 
  -- listColumns

  let csv = "38"
      req = appendBody (BL.pack csv)
         (makeRequest tok fusiontableApi "POST"
           (fusiontableHost, "fusiontables/v1/tables/"++tid++"/import" ))
  
  error "implement bulkImportRows"


-- TODO: provide some basic select functionality
filterRows = error "implement filterRows"
