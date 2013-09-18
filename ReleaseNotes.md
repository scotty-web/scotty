## 0.5.0

* The Scotty monads (`ScottyM` and `ActionM`) are now monad transformers,
  allowing Scotty applications to be embedded in arbitrary `MonadIO`s.
  The old API continues to be exported from `Web.Scotty` where:

    type ScottyM = ScottyT IO
    type ActionM = ActionT IO

  The new transformers are found in `Web.Scotty.Trans`. See the
  `globalstate` example for use. Special thanks to Dan Frumin (co-dan)
  for much of the legwork here.

* Added support for HTTP PATCH method.

* Removed lambda action syntax. This will return when we have a better
  story for typesafe routes.

* `reqHeader :: Text -> ActionM Text` ==> 
  `reqHeader :: Text -> ActionM (Maybe Text)`

* New `raw` method to set body to a raw `ByteString`

* Parse error thrown by `jsonData` now includes the body it couldn't parse.

* `header` split into `setHeader` and `addHeader`. The former replaces
  a response header (original behavior). The latter adds a header (useful
  for multiple `Set-Cookie`s, for instance.
