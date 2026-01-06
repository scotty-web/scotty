## 0.30 [2026.01.06]

* Added sessions (#317).
* Fixed cookie example from `Cookie` module documentation. `getCookie` Function would return strict variant of `Text`. Will convert it into lazy variant using `fromStrict`. 
* Exposed simple functions of `Cookie` module via `Web.Scotty` & `Web.Scotty.Trans`.
* Add tests for URL encoding of query parameters and form parameters. Add `formData` action for decoding `FromForm` instances (#321).
* Add explicit redirect functions for all redirect status codes.
* Added ActionM variants for cookie module functions (#406).
* Updated `Parsable` instance for `Bool` to accept `on` for HTML form checkboxes.
* Add tests demonstrating usage of `Network.Wai.Middleware.ValidateHeaders` from wai-extra for header validation (#359).
* Added `jsonMode` flag to `Options` to return JSON-formatted error responses instead of HTML (#97). When enabled, all default error handlers (404, 500, etc.) return JSON with format `{"status": <code>, "description": "<message>"}`. Content-Type is set to `application/json; charset=utf-8`.
* Added middleware example demonstrating request logging and request header validation using WAI middlewares (#199, #385).

### Breaking changes
* Remove dependency on data-default class (#386). We have been exporting constants for default config values since 0.20, and this dependency was simply unnecessary.
* Remove re-export of `Network.Wai.Parse.ParseRequestBodyOptions` from `Web.Scotty` and `Web.Scotty.Trans`. This is a type from `wai-extra` so exporting it is unnecessary.
* Remove deprecated exports (#408) `liftAndCatchIO`, `param`, `params`, `raise`, `raiseStatus`, `rescue` from `Web.Scotty` and `Web.Scotty.Trans`.
* Remove deprecated `StatusError` type from `Scotty.Internal.Types`.
* Remove typeclass instance `MonadError StatusError (ActionT m)` from `Scotty.Internal.Types`.

## 0.22 [2024.03.09]

### New
* add `instance Parsable UTCTime` (#250)
* add `filesOpts` (#369). Form parameters and files are only parsed from the request body if needed; `filesOpts` introduces options to customize upload limits, a mechanism to back uploads with temporary files based on resourcet, as well as a continuation-based syntax to process such temporary files. 

### Fixes
* `files` does not accept unbounded uploads anymore (see #183, #203), but like `filesOpts` it is backed by temporary files which are read back in memory and removed right away. The limits for `files` are prescribed by `defaultParseBodyOptions` in wai-extra (#369).
* Path parameters with value matching the parameter name prefixed by colon will properly populate `pathParams` with their literal value : `/:param` will match `/:param` and add a `Param` with value `("param", ":param")` (#301)
* Accept text-2.1 (#364)
* Remove dependency upper bounds on `text` and `bytestring` (#371)
* When in 'verbose' mode any unhandled exceptions are printed to stderr as well (#374)

### Breaking changes
* some ActionT API functions have now a MonadIO or MonadUnliftIO constraint rather than Monad reflecting that there is where request reading takes place. E.g. `files` has now a MonadUnliftIO constraint on its base monad. (#369)
* the File type has now a type parameter to reflect whether it carries file contents or just a filepath pointing to the temp file (#369).



## 0.21 [2023.12.17]
### New
* add `getResponseHeaders`, `getResponseStatus`, `getResponseContent` (#214)
* add `captureParamMaybe`, `formParamMaybe`, `queryParamMaybe` (#322)
* add `Web.Scotty.Trans.Strict` and `Web.Scotty.Trans.Lazy` (#334)
* renamed `captureParam`, `captureParamMaybe`, and `captureParams` to `pathParam`, `pathParamMaybe`, `pathParams` respectively, keeping the old names as their synonyms (#344)

### Deprecated
* deprecate `rescue` and `liftAndCatchIO` (#332)
* Deprecate `StatusError`, `raise` and `raiseStatus` (#351)

### Fixes
* Reverted the `MonadReader` instance of `ActionT` so that it inherits the base monad (#342)
* Scotty's API such as `queryParam` now throws `ScottyException` rather than `StatusException`.
  Uncaught exceptions are handled by `scottyExceptionHandler`, resembling the existing behaviour
  
### Breaking changes
* `File` type: the first component of the tuple is strict text now (used to be lazy prior to 0.21) (#370)

### Documentation
* Add doctest, refactor some inline examples into doctests (#353)
* document "`defaultHandler` only applies to endpoints defined after it" (#237)



## 0.20.1 [2023.10.03]
* remove dependencies on 'base-compat' and 'base-compat-batteries' (#318)
* re-add MonadFail (ActionT m) instance (#325)
* re-add MonadError (ActionT m) instance, but the error type is now specialized to 'StatusError' (#325)
* raise lower bound on base ( > 4.14 ) to reflect support for GHC >= 8.10 (#325).


## 0.20 [2023.10.02]
* Drop support for GHC < 8.10 and modernise the CI pipeline (#300).
* Adds a new `nested` handler that allows you to place an entire WAI Application under a Scotty route (#233).
* Disambiguate request parameters (#204). Adjust the `Env` type to have three `[Param]` fields instead of one, add `captureParam`, `formParam`, `queryParam` and the associated `captureParams`, `formParams`, `queryParams`. Add deprecation notices to `param` and `params`.
* Add `Scotty.Cookie` module (#293).
* Change body parsing behaviour such that calls to `next` don't result in POST request bodies disappearing (#147).
* (Internal) Remove unused type `RequestBodyState` (#313)
* Rewrite `ActionT` using the "ReaderT pattern" (#310) https://www.fpcomplete.com/blog/readert-design-pattern/

Breaking:

* (#310) Introduce `unliftio` as a dependency, and base exception handling on `catch`.
* (#310) Clarify the exception handling mechanism of ActionT, ScottyT. `rescue` changes signature to use proper `Exception` types rather than strings. Remove `ScottyError` typeclass.
* (#310) All ActionT methods (`text`, `html` etc.) have now a MonadIO constraint on the base monad rather than Monad because the response is constructed in a TVar inside ActionEnv. `rescue` has a MonadUnliftIO constraint. The Alternative instance of ActionT also is based on MonadUnliftIO because `<|>` is implemented in terms of `catch`. `ScottyT` and `ActionT` do not have an exception type parameter anymore.
* (#310) MonadError e (ActionT m) instance removed
* (#310) MonadFail (ActionT m) instance is missing by mistake.

## 0.12.1 [2022.11.17]
* Fix CPP bug that prevented tests from building on Windows.
* Allow building with `transformers-0.6.*` and `mtl-2.3.*`. Because the
  `MonadTrans t` class gained a `forall m. Monad m => Monad (t m)` superclass
  in `transformers-0.6.0.0`, the `MonadTrans` and `MonadTransControl` instances
  for `ActionT e` now have a `ScottyError e` instance context to match the
  `Monad` instance.

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
