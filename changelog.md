## 0.12 [2020.05.16]
* Provide `MonadReader` and `MonadState` instances for `ActionT`.
* Add HTTP Status code as a field to `ActionError`, and add
  a sister function to `raise`, `raiseStatus`. This makes
  throwing a specific error code and exiting much cleaner, and
  avoids the strange defaulting to HTTP 500. This will make internal
  functions easier to implement with the right status codes 'thrown',
  such as `jsonData`.
* Correct http statuses returned by `jsonData` (#228).
* Better error message when no data is provided to `jsonData` (#226).
* Add `Semigroup` and `Monoid` instances for `ActionT` and `ScottyT`
* ScottyT: Use strict StateT instead of lazy
* Handle adjacent slashes in the request path as one (thanks @SkyWriter)

## 0.11.5 [2019.09.07]
* Allow building the test suite with `hspec-wai-0.10`.

## 0.11.4 [2019.05.02]
* Allow building with `base-4.13` (GHC 8.8).

## 0.11.3 [2019.01.08]
* Drop the test suite's dependency on `hpc-coveralls`, which is unmaintained
  and does not build with GHC 8.4 or later.

## 0.11.2 [2018.07.02]
* Migrate from `Network` to `Network.Socket` to avoid deprecation warnings.

## 0.11.1 [2018.04.07]
* Add `MonadThrow` and `MonadCatch` instances for `ActionT` [abhinav]
* Fix `matchAny` so that all methods are matched, not just standard ones
  [taphu]

## 0.11.0
* IO exceptions are no longer automatically turned into ScottyErrors by
  `liftIO`. Use `liftAndCatchIO` to get that behavior.
* New `finish` function.
* Text values are now leniently decoded from ByteStrings.
* Added `MonadFail` instance for `ScottyT`
* Lots of bound bumps on dependencies.

## 0.10.2
* Removed debug statement from routes

## 0.10.1
* `Parsable` instances for `Word`, `Word8`, `Word16`, `Word32`, `Word64`
   [adamflott]
* `Parsable` instances for `Int8`, `Int16`, `Int32`, `Int64`, and `Natural`
* Removed redundant `Monad` constraint on `middleware`

## 0.10.0

* The monad parameters to `ScottyT` have been decoupled, causing the type
  of the `ScottyT` constructor to change. As a result, `ScottyT` is no
  longer a `MonadTrans` instance, and the type signatures of`scottyT`,
  `scottyAppT`, and `scottyOptsT` have been simplified. [ehamberg]

* `socketDescription` no longer uses the deprecated `PortNum` constructor.
  Instead, it uses the `Show` instance for `PortNumber`. This changes the
  bytes from host to network order, so the output of `socketDescription`
  could change. [ehamberg]

* `Alternative`, `MonadPlus` instances for `ActionT`

* `scotty` now depends on `transformers-compat`. As a result, `ActionT` now
  uses `ExceptT`, regardless of which version of `transformers` is used.
  As a result, several functions in `Web.Scotty.Trans` no longer require a
  `ScottyError` constraint, since `ExceptT` does not require an `Error`
  constraint (unlike `ErrorT`).

* Added support for OPTIONS routes via the `options` function [alvare]

* Add `scottySocket` and `scottySocketT`, exposing Warp Unix socket support
  [hakujin]

* `Parsable` instance for lazy `ByteString` [tattsun]

* Added streaming uploads via the `bodyReader` function, which retrieves chunks
  of the request body. [edofic]
  - `ActionEnv` had a `getBodyChunk` field added (in
    `Web.Scotty.Internal.Types`)
  - `RequestBodyState` and `BodyPartiallyStreamed` added to
    `Web.Scotty.Internal.Types`

* `jsonData` uses `aeson`'s `eitherDecode` instead of just `decode` [k-bx]

## 0.9.1

* text/html/json only set Content-Type header when not already set

## 0.9.0

* Add `charset=utf-8` to `Content-Type` for `text`, `html` and `json`

* Assume HTTP status 500 for `defaultHandler`

* Remove deprecated `source` method.

* No longer depend on conduit.

## 0.8.2

* Bump `aeson` upper bound

* Fix `mtl` related deprecation warnings

## 0.8.1

* Export internal types

* Added `MonadBase`, `MonadTransControl` and `MonadBaseControl` instances for
  `ActionT`

## 0.8.0

* Upgrade to wai/wai-extra/warp 3.0

* No longer depend on conduit-extra.

* The `source` response method has been deprecated in favor
  of a new `stream` response, matching changes in WAI 3.0.

* Removed the deprecated `reqHeader` function.

## 0.7.3

* Bump upper bound for case-insensitive, mtl and transformers.

## 0.7.2

* Bump lower bound on conduit, add conduit-extra to cabal build depends.

## 0.7.1

* Default warp settings now use `setFdCacheDuration 0` to work around a warp
  issue where file changes are not getting picked up.

## 0.7.0

* Renamed `reqHeader` to `header`. Added `headers` function to get all headers.

* Changed `MonadIO` instance for `ActionT` such that IO exceptions are lifted
  into `ScottyError`s via `stringError`.

* Make `Bool` parsing case-insensitive. Goal: support both Haskell's True/False
  and Javascript's true/false. Thanks to Ben Gamari for suggesting this.

* Bump `aeson`/`text` upper bounds.

* Bump `wai`/`wai-extra`/`warp` bounds, including new lower bound for `warp`, which fixes a security issue related to Slowloris protection.

## 0.6.2

* Bump upper bound for `text`.

## 0.6.1

* Match changes in `wai-extra`.

## 0.6.0

* The Scotty transformers (`ScottyT` and `ActionT`) are now parameterized
  over a custom exception type, allowing one to extend Scotty's `ErrorT`
  layer with something richer than `Text` errors. See the `exceptions`
  example for use. `ScottyM` and `ActionM` remain specialized to `Text`
  exceptions for simplicity.

* Both monads are now instances of `Functor` and `Applicative`.

* There is a new `cookies` example.

* Internals brought up-to-date with WAI 2.0 and related packages.

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
  for multiple `Set-Cookie`s, for instance).
