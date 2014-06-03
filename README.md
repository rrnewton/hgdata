
This project provides a Haskell library and command-line interface for Google services such as Google Storage, Contacts, Books, etc. The package and on-line documentation is available from [Hackage](http://hackage.haskell.org/package/handa-gdata).

For OAuth 2.0, the following operations are supported:

 * [Forming a URL](https://developers.google.com/accounts/docs/OAuth2InstalledApp#formingtheurl) for authorizing [one or more Google APIs](https://developers.google.com/oauthplayground/)
 * [Exchanging an authorization code for tokens](https://developers.google.com/accounts/docs/OAuth2InstalledApp#handlingtheresponse)
 * [Refreshing tokens](https://developers.google.com/accounts/docs/OAuth2InstalledApp#refresh)
 * [Validating tokens](https://developers.google.com/accounts/docs/OAuth2Login#validatingtoken)

For the [Google Storage API](https://developers.google.com/storage/docs/reference-methods), the following operations are supported:

 * [GET Service](https://developers.google.com/storage/docs/reference-methods#getservice)
 * [PUT Bucket](https://developers.google.com/storage/docs/reference-methods#putbucket)
 * [GET Bucket](https://developers.google.com/storage/docs/reference-methods#getbucket)
 * [DELETE Bucket](https://developers.google.com/storage/docs/reference-methods#deletebucket)
 * [GET Object](https://developers.google.com/storage/docs/reference-methods#getobject)
 * [PUT Object](https://developers.google.com/storage/docs/reference-methods#putobject)
 * [HEAD Object](https://developers.google.com/storage/docs/reference-methods#headobject)
 * [DELETE Object](https://developers.google.com/storage/docs/reference-methods#deleteobject)

Operations in the [Google Fusion Tables API](https://developers.google.com/fusiontables/) are also supported.

For the [unofficial Google Bookmarks API](http://www.mmartins.com/mmartins/googlebookmarksapi), the following operations are supported:

 * List bookmarks
 
For the [Google Books API](https://developers.google.com/books/docs/v1/using), the following operations are supported:

 * [List bookshelves in My Library](https://developers.google.com/books/docs/v1/using#RetrievingMyBookshelves)
 * [List books in My Library](https://developers.google.com/books/docs/v1/using#RetrievingMyBookshelfVolumes)
 
For the [Google Contacts API](https://developers.google.com/google-apps/contacts/v3/), the following operations are supported:

 * Downloading a full list of contacts in XML format
 * Extracting and decrypting GnuPG/PGP text in contacts' Notes fields

For the [Picasa API](https://developers.google.com/picasa-web/docs/2.0/developers_guide_protocol), the following operations are supported:

 * [Listing albums](https://developers.google.com/picasa-web/docs/2.0/developers_guide_protocol#ListAlbums)
 * [Listing photos in an album](https://developers.google.com/picasa-web/docs/2.0/developers_guide_protocol#ListAlbumPhotos)

In the future we will add support for the following APIs:

 * [Google Drive](https://developers.google.com/drive/v2/reference/)
 
The command-line application provides the following capabilities:

 * OAuth 2.0
  - Generate an OAuth 2.0 URL
  - Exchange an OAuth 2.0 code for tokens
  - Refresh OAuth 2.0 tokens
 * Bookmarks
  _ List bookmarks
 * Books
  - List bookshelves
  - List books
 * Contacts
  - Download Google Contacts
  - Decrypt GnuPG/PGP content in the Notes fields
 * Picasa
  - List albums
  - List photos
 * Storage
  - List objects in a Google Storage bucket
  - Get an object from a Google Storage bucket
  - Put an object into a Google Storage bucket
  - Delete an object from a Google Storage bucket
  - Get object metadata from a Google Storage bucket
  - Synchronize a directory with a Google Storage bucket
  
