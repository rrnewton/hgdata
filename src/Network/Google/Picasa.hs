-----------------------------------------------------------------------------
--
-- Module      :  Network.Google.Picasa
-- Copyright   :  (c) 2012-13 Brian W Bush
-- License     :  MIT
--
-- Maintainer  :  Brian W Bush <b.w.bush@acm.org>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Functions for accessing the Picasa API, see <https://developers.google.com/picasa-web/docs/2.0/developers_guide_protocol>.
--
-----------------------------------------------------------------------------


module Network.Google.Picasa (
-- * Types
  UserId
, defaultUser
, AlbumId
-- * Functions
, listAlbums
, listPhotos
) where


import Control.Monad (liftM)
import Data.Maybe (mapMaybe)
import Network.Google (AccessToken, ProjectId, doRequest, makeRequest)
import Network.HTTP.Conduit (Request)
import Text.XML.Light (Element(elContent), QName(..), filterChildrenName, findChild, strContent)


-- | The host for API access.
picasaHost :: String
picasaHost = "picasaweb.google.com"


-- | The API version used here.
picasaApi :: (String, String)
picasaApi = ("Gdata-version", "2")


-- | Picasa user ID.
type UserId = String


-- | Default Picasa user ID
defaultUser :: UserId
defaultUser = "default"


-- | Picasa album ID.
type AlbumId = String


-- | List the albums, see <https://developers.google.com/picasa-web/docs/2.0/developers_guide_protocol#ListAlbums>.
listAlbums ::
     AccessToken  -- ^ The OAuth 2.0 access token.
  -> UserId       -- ^ The user ID for the photos.
  -> IO Element   -- ^ The action returning the contacts in XML format.
listAlbums accessToken userId =
  doRequest $ picasaFeedRequest accessToken userId Nothing


-- | Extract the album IDs from the list of albums.
extractAlbumIds ::
     Element    -- ^ The root element of the list of albums.
  -> [AlbumId]  -- ^ The list of album IDs.
extractAlbumIds root =
  let
    idQname = QName {qName = "id", qURI = Just "http://schemas.google.com/photos/2007", qPrefix = Just "gphoto"}
    entries :: [Element]
    entries = filterChildrenName ((== "entry") . qName) root
    extractAlbumId :: Element -> Maybe String
    extractAlbumId = liftM strContent . findChild idQname
  in
    mapMaybe extractAlbumId entries


-- | List the photos in albums, see <https://developers.google.com/picasa-web/docs/2.0/developers_guide_protocol#ListAlbumPhotos>.
listPhotos ::
     AccessToken  -- ^ The OAuth 2.0 access token.
  -> UserId       -- ^ The user ID for the photos.
  -> [AlbumId]    -- ^ The album ID for the photos, or all photos if null.
  -> IO Element   -- ^ The action returning the contacts in XML format.
listPhotos accessToken userId albumIds =
  do
    albumIds' <-
      if null albumIds
        then liftM extractAlbumIds $ listAlbums accessToken userId
        else return albumIds
    results <- mapM (listAlbumPhotos accessToken userId) albumIds'
    let
      root = head results
    return $ root {elContent = concatMap elContent results}


-- | List the photos in an album, see <https://developers.google.com/picasa-web/docs/2.0/developers_guide_protocol#ListAlbumPhotos>.
listAlbumPhotos ::
     AccessToken  -- ^ The OAuth 2.0 access token.
  -> UserId       -- ^ The user ID for the photos.
  -> AlbumId      -- ^ The album ID for the photos.
  -> IO Element   -- ^ The action returning the contacts in XML format.
listAlbumPhotos accessToken userId albumId =
  doRequest $ picasaFeedRequest accessToken userId (Just albumId)


-- | Make an HTTP request for a Picasa feed.
picasaFeedRequest ::
     AccessToken    -- ^ The OAuth 2.0 access token.
  -> UserId         -- ^ The user ID for the photos.
  -> Maybe AlbumId  -- ^ The album ID for the photos.
  -> Request m      -- ^ The request.
picasaFeedRequest accessToken userId albumId =
  makeRequest accessToken picasaApi "GET"
    (
      picasaHost
    , "/data/feed/api/user/" ++ userId ++ maybe "" ("/albumid/" ++) albumId
    )
