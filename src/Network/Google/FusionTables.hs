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
    
  -- * One-to-one wrappers around API routines, with parsing of JSON results
  , createTable, createColumn
  , listTables, listColumns
--  , sqlQuery
    
  -- * Higher level interface to common SQL queries    
  , insertRows
    -- , filterRows
  , bulkImportRows
    
  -- js experimentation
  , getData, ColData(..), FTValue(..), tableSelect
             
) where

import           Control.Monad (liftM, unless)
import           Data.Maybe (mapMaybe)
import           Data.List as L
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Network.Google (AccessToken, ProjectId, doRequest, makeRequest, appendBody, appendHeaders)
import           Network.HTTP.Conduit (Request(..), RequestBody(..),parseUrl)
import qualified Network.HTTP as H
import           Text.XML.Light (Element(elContent), QName(..), filterChildrenName, findChild, strContent)

-- TODO: Ideally this dependency wouldn't exist here and the user could select their
-- own JSON parsing lib (e.g. Aeson).
import           Text.JSON (JSObject(..), JSValue(..), Result(Ok,Error),
                            decode, valFromObj, toJSObject, toJSString, fromJSString,readJSON)
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
createTable :: AccessToken -> String -> [(FTString,CellType)] -> IO TableMetadata
createTable tok name cols =
  do response <- doRequest req
     let Ok final = parseTable response
     return final     
 where
   req = appendHeaders [("Content-Type", "application/json")] $
          appendBody (BL.pack json)
          (makeRequest tok fusiontableApi "POST"
            (fusiontableHost, "fusiontables/v1/tables" ))
   json :: String
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

-- | Create a new column in a given table.  Returns the metadata for the new column.
createColumn :: AccessToken -> TableId -> (FTString,CellType) -> IO ColumnMetadata
createColumn tok tab_tableId (colName,colTy) =
  do response <- doRequest req
     let Ok final = parseColumn response
     return final
 where
   req = appendHeaders [("Content-Type", "application/json")] $
          appendBody (BL.pack json)
          (makeRequest tok fusiontableApi "POST"
            (fusiontableHost, "fusiontables/v1/tables/"++tab_tableId++"/columns" ))
   json :: String
   json = render$ pp_value$ JSObject$ toJSObject$
          [ ("name",str colName)
          , ("type",str$ show colTy) ]
   str = JSString . toJSString


-- | Designed to mirror the types listed here:
--   <https://developers.google.com/fusiontables/docs/v1/reference/column>
data CellType = NUMBER | STRING | LOCATION | DATETIME
  deriving (Show,Eq,Ord,Read)

-- | List all tables belonging to a user.
--   See <https://developers.google.com/fusiontables/docs/v1/reference/table/list>.
listTables :: AccessToken -- ^ The OAuth 2.0 access token.
           -> IO [TableMetadata]
listTables accessToken =
  do resp <- doRequest req
     case parseTables resp of
       Ok x -> return x
       Error err -> error$ "listTables: failed to parse JSON response, error was:\n  "
                    ++err++"\nJSON response was:\n  "++show resp
 where
   req  = makeRequest accessToken fusiontableApi "GET"
                     ( fusiontableHost, "fusiontables/v1/tables" )


-- | Construct a simple Haskell representation of the result of `listTables`.
parseTables :: JSValue -> Result [TableMetadata]
parseTables (JSObject ob) = do
  JSArray allTables <- valFromObj "items" ob
  mapM parseTable allTables

parseTable :: JSValue -> Result TableMetadata
parseTable (JSObject ob) = do
  tab_name     <- valFromObj "name"     ob
  tab_tableId  <- valFromObj "tableId" ob
  tab_columns  <- mapM parseColumn =<< valFromObj "columns" ob
  return TableMetadata {tab_name, tab_tableId, tab_columns}
parseTable oth = Error$ "parseTable: Expected JSObject, got "++show oth
   
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
            -> IO [ColumnMetadata]
listColumns accessToken tid = 
  do resp <- doRequest req
     case parseColumns resp of
       Ok x -> return x
       Error err -> error$ "listColumns: failed to parse JSON response:\n"++err
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
-- NOTE: this method has a major limitation.  SQL queries are encoded into the URL,
-- and it is very easy to exceed the maximum URL length accepted by Google APIs.
insertRows :: AccessToken -> TableId
              -> [FTString]   -- ^ Which columns to write.
              -> [[FTString]] -- ^ Rows 
              -> IO ()
insertRows tok tid cols rows = doRequest req     
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
--
-- This function also checks that the server reports receiving the same number of
-- rows as were uploaded.
--
-- NOTE: also use this function for LONG rows, even if there is only one.  Really,
-- you should almost always use this function rather thna `insertRows`.
bulkImportRows :: AccessToken -> TableId
              -> [FTString]   -- ^ Which columns to write.
              -> [[FTString]] -- ^ Rows 
              -> IO ()
bulkImportRows tok tid cols rows = do 
  targetSchema <- fmap (map col_name) $ listColumns tok tid

  unless (targetSchema == cols) $ 
    error$ "bulkImportRows: upload schema (1) did not match server side schema (2):\n (1) "++
           show cols ++"\n (2) " ++ show targetSchema

  let csv = unlines rowlns -- (header : rowlns)  -- ARGH, they don't accept the header.
                           -- All sanity checking must be client side [2013.11.30].
      -- header = concat$ intersperse "," $ map show cols
      rowlns = [ concat$ intersperse "," [ "\"" ++f++"\"" | f <- row ]
               | row <- rows ]
      req = appendBody (BL.pack csv) $ 
         (makeRequest tok fusiontableApi "POST"
           (fusiontableHost, "upload/fusiontables/v1/tables/"++tid++"/import" ))
         {
           queryString = B.pack $ H.urlEncodeVars [("isStrict", "true")]
         }
  resp <- doRequest req
  let Ok received = parseResponse resp  
  unless (received == length rows) $
    error$ "attempted to upload "++show (length rows)++
           " rows, but "++show received++" received by server."
  return ()

 where 
   parseResponse :: JSValue -> Result Int
   parseResponse (JSObject ob) = do
   --  JS allTables <- valFromObj "items" ob
--   JSString "fusiontables#import" <- valFromObj "kind" ob -- Sanity check.
   JSString num <- valFromObj "numRowsReceived" ob 
   return$ read$ fromJSString num


-- TODO: provide some basic select functionality
filterRows = error "implement filterRows"

--getData :: AccessToken
--     -> String
--     -> String
--     -> Request m

getData = tableSelect

tableSelect token table_id str
  = let req = (makeRequest token fusiontableApi "GET"
              (fusiontableHost, "/fusiontables/v1/query"))
              { 
                queryString = B.pack$ H.urlEncodeVars [("sql",query)] 
              } 
        query = "SELECT " ++ str ++ " FROM " ++ table_id
    in do resp <- doRequest req
          case parseResponse resp of
            Ok x -> return x
            Error err -> error$ "getData: failed to parse JSON response:\n"++err
  where
    parseResponse :: JSValue -> Result ColData
    parseResponse (JSArray as) = Error "GOT ARRAY EARLY" 
    parseResponse (JSObject ob) = do
      -- get array of column names (headings) 
      (JSArray cols)  <- valFromObj "columns" ob
      let colNom = map (\(JSString s) -> fromJSString s) cols 
      -- Get array of array of data values 
      (JSArray rows)  <- valFromObj "rows" ob
      rows' <- mapM parseVal' rows 
      return $ ColData colNom rows'
    parseVal' (JSArray ar) = mapM parseVal ar 
    parseVal :: JSValue -> Result FTValue
    parseVal r@(JSRational _ _) = do
      d <- readJSON r
      return $ DoubleValue d
    parseVal (JSString s)     = return $ StringValue $ fromJSString s 
    
    parseVal _ = Error "I imagined there'd be Rationals here"  


data FTValue = StringValue FTString 
             | DoubleValue Double
             deriving Show 
               


data ColData = ColData {colName :: [FTString],
                        values  :: [[FTValue]]}
               deriving Show

