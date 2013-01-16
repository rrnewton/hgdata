-----------------------------------------------------------------------------
--
-- Module      :  Network.Google.Books
-- Copyright   :  (c) 2012-13 Brian W Bush
-- License     :  MIT
--
-- Maintainer  :  Brian W Bush <b.w.bush@acm.org>
-- Stability   :  Stable
-- Portability :
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


import Network.Google (AccessToken, doRequest, makeRequest)
import Network.HTTP.Conduit (Request)
import Text.JSON (JSValue(JSArray))


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
  -> IO JSValue   -- ^ The action returning the albums metadata in XML format.
listBookshelves accessToken =
  doRequest $ booksRequest accessToken Nothing


-- | List the books, see <https://developers.google.com/books/docs/v1/using#RetrievingMyBookshelfVolumes>.
listBooks ::
     AccessToken  -- ^ The OAuth 2.0 access token.
  -> [ShelfId]    -- ^ The bookshelf IDs.
  -> IO JSValue   -- ^ The action returning the albums metadata in XML format.
listBooks accessToken shelves =
  do
    results <- mapM (listShelfBooks accessToken) shelves
    return $ JSArray results


-- | List the books in a shelf, see <https://developers.google.com/books/docs/v1/using#RetrievingMyBookshelfVolumes>.
listShelfBooks ::
     AccessToken  -- ^ The OAuth 2.0 access token.
  -> ShelfId      -- ^ The bookshelf ID.
  -> IO JSValue   -- ^ The action returning the albums metadata in XML format.
listShelfBooks accessToken shelf =
  doRequest $ booksRequest accessToken (Just shelf)


-- | Make an HTTP request for Google Books.
booksRequest ::
     AccessToken    -- ^ The OAuth 2.0 access token.
  -> Maybe ShelfId  -- ^ The bookshelf ID.
  -> Request m      -- ^ The request.
booksRequest accessToken shelf =
  makeRequest accessToken booksApi "GET"
    (
      booksHost
    , "/books/v1/mylibrary/bookshelves" ++ maybe "" (\x -> "/" ++ x ++ "/volumes") shelf
    )
