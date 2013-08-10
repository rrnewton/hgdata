-----------------------------------------------------------------------------
--
-- Module      :  Network.Google.Books
-- Copyright   :  (c) 2012-13 Brian W Bush
-- License     :  MIT
--
-- Maintainer  :  Brian W Bush <b.w.bush@acm.org>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Functions for the Google Books API, see <https://developers.google.com/books/docs/v1/using#WorkingMyBookshelves>.
--
-----------------------------------------------------------------------------


module Network.Google.Books (
-- * Types
  ShelfId
-- * Functions
, listBooks
, listBookshelves
) where


import Control.Monad (liftM)
import Data.Maybe (fromMaybe)
import Network.Google (AccessToken, appendQuery, doRequest, makeRequest)
import Network.HTTP.Conduit (Request)
import Text.JSON (JSObject, JSValue(..), Result(Ok), decode, fromJSObject, toJSObject, valFromObj)


-- | The host for API access.
booksHost :: String
booksHost = "www.googleapis.com"


-- | The API version used here.
booksApi :: (String, String)
booksApi = ("Gdata-version", "2")


-- | Bookshelf ID.
type ShelfId = String


-- | List the bookshelves, see <https://developers.google.com/books/docs/v1/using#RetrievingMyBookshelves>.
listBookshelves ::
     AccessToken  -- ^ The OAuth 2.0 access token.
  -> IO JSValue   -- ^ The action returning the bookshelves' metadata in JSON format.
listBookshelves accessToken =
  doRequest $ booksRequest accessToken Nothing 0


-- | List the bookshelf IDs, see <https://developers.google.com/books/docs/v1/using#RetrievingMyBookshelves>.
listBookshelfIds ::
     AccessToken  -- ^ The OAuth 2.0 access token.
  -> IO [String]  -- ^ The action returning list of bookshelf IDs.
listBookshelfIds accessToken =
  do
    JSObject result <- listBookshelves accessToken
    let
      extractId :: JSValue -> String
      extractId (JSObject x) =
        let
          y :: Rational
          Ok (JSRational _ y) = valFromObj "id" x
        in
          show (round y :: Int)
      items :: [JSValue]
      Ok (JSArray items) = valFromObj "items" result
    return $ map extractId items


-- | List the books, see <https://developers.google.com/books/docs/v1/using#RetrievingMyBookshelfVolumes>.
listBooks ::
     AccessToken  -- ^ The OAuth 2.0 access token.
  -> [ShelfId]    -- ^ The bookshelf IDs.
  -> IO JSValue   -- ^ The action returning the books' metadata in JSON format.
listBooks accessToken shelves =
  do
    shelves' <-
      if null shelves
        then listBookshelfIds accessToken
        else return shelves
    results <- mapM (listShelfBooks accessToken) shelves'
    return $ JSArray results


-- | List the books in a shelf, see <https://developers.google.com/books/docs/v1/using#RetrievingMyBookshelfVolumes>.
listShelfBooks ::
     AccessToken  -- ^ The OAuth 2.0 access token.
  -> ShelfId      -- ^ The bookshelf ID.
  -> IO JSValue   -- ^ The action returning the books' metadata in JSON format.
listShelfBooks accessToken shelf =
  do
    x <- listShelfBooks' accessToken shelf Nothing
    let
      y :: [JSValue]
      y = concatMap items x
      JSObject o = head x
      z :: [(String, JSValue)]
      z = fromJSObject o
      u :: [(String, JSValue)]
      u = filter (\w -> fst w /= "items") z
      v :: [(String, JSValue)]
      v = ("items", JSArray y) : u
    return $ JSObject $ toJSObject v


-- | List the books in a shelf, see <https://developers.google.com/books/docs/v1/using#RetrievingMyBookshelfVolumes>.
listShelfBooks' ::
     AccessToken    -- ^ The OAuth 2.0 access token.
  -> ShelfId        -- ^ The bookshelf ID.
  -> Maybe Int      -- ^ The start index in the list of metadata.
  -> IO [JSValue]   -- ^ The action returning the books' metadata in JSON format.
listShelfBooks' accessToken shelf startIndex =
  do
    let
      startIndex' :: Int
      startIndex' = fromMaybe 0 startIndex
    books <- doRequest $ booksRequest accessToken (Just shelf) startIndex'
    let
      startIndex'' = startIndex' + length (items books)
    liftM (books :) $
      if startIndex' + 1 <= totalItems books
        then listShelfBooks' accessToken shelf $ Just startIndex''
        else return []


-- | Find the total number of items in a shelf.
totalItems ::
     JSValue  -- ^ The books' metadata.
  -> Int      -- ^ The total number of books in the shelf.
totalItems (JSObject books) =
  let
    Ok count = "totalItems" `valFromObj` books
  in
    count


-- | Find the items in a list of books' metadata.
items ::
     JSValue    -- ^ The books' metadata
  -> [JSValue]  -- ^ The books in the metadata.
items (JSObject books) =
  let
    list = "items" `valFromObj` books
    f (Ok x) = x
    f _ = []
  in
    f list


-- | Make an HTTP request for Google Books.
booksRequest ::
     AccessToken    -- ^ The OAuth 2.0 access token.
  -> Maybe ShelfId  -- ^ The bookshelf ID.
  -> Int            -- ^ The starting index
  -> Request m      -- ^ The request.
booksRequest accessToken shelf startIndex =
  appendQuery
    [
      ("maxResults", "40")
    , ("startIndex", show startIndex)
    ]
    $
    makeRequest accessToken booksApi "GET"
    (
      booksHost
    , "/books/v1/mylibrary/bookshelves" ++ maybe "" (\x -> "/" ++ x ++ "/volumes") shelf

    )
