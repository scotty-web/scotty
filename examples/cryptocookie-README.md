# wai-cryptocookie + Scotty Integration Example

This is a working example demonstrating encrypted session cookies using wai-cryptocookie with Scotty.

## What This Example Does

- Sets up wai-cryptocookie (v0.3) with AES-256-GCM-SIV encryption
- Integrates wai-csrf (v0.1) for CSRF protection
- Demonstrates proper middleware composition
- Shows encryption key management

## Running the Example

```bash
cabal run scotty-cryptocookie
```

Then visit: http://localhost:3000/

## Architecture

```
Client Request
     ↓
[wai-csrf middleware]
  - Validates CSRF token from cookie/header
  - Generates new token if needed
  - Passes Token to next middleware
     ↓
[wai-cryptocookie middleware]
  - Uses CSRF token as AEAD associated data
  - Decrypts session cookie (if present)
  - Makes decrypted session available
     ↓
[Scotty Application]
  - Access session via middleware
  - Set new encrypted sessions
     ↓
Response with encrypted cookies
```

## Complete Setup Code

```haskell
-- 1. Initialize encryption key (32 bytes for AES-256)
sessionKey <- CC.autoKeyFileBase16 @"AEAD_AES_256_GCM_SIV" "key.txt"

-- 2. Create cryptocookie environment
let cryptoConfig = CC.Config
        { CC.cookieName = "SESSION"
        , CC.key = sessionKey
        , CC.aadEncode = \(Wai.CSRF.Token t) -> BL.fromStrict $ BA.convert t
        , CC.msgEncode = Ae.encode
        , CC.msgDecode = Ae.decode
        }
cryptoEnv <- CC.newEnv cryptoConfig

-- 3. Configure CSRF
let csrfConfig = Wai.CSRF.defaultConfig
        { Wai.CSRF.cookieName = "CSRF-TOKEN"
        , Wai.CSRF.headerName = "X-CSRF-Token"
        }

-- 4. Build Scotty app
app <- scottyApp $ do
    get "/" $ text "Hello!"

-- 5. Compose middleware (CSRF first, then cryptocookie)
let fullApp =
      Wai.CSRF.middleware csrfConfig $ \mCsrfToken ->
        CC.middleware cryptoEnv handleSession mCsrfToken
  where
    handleSession :: Maybe (Wai.CSRF.Token, Maybe UserSession)
                  -> Wai.Application
    handleSession mData = app

-- 6. Run server
Warp.run 3000 fullApp
```

## Setting an Encrypted Cookie

```haskell
-- Inside a Scotty action with access to CSRF token:
let session = UserSession
        { userId = 42
        , username = "alice"
        , role = "admin"
        }

-- Encrypt session with CSRF token as AAD
cookie <- liftIO $ CC.setCookie cryptoEnv csrfToken session

-- Add to response
addHeader "Set-Cookie" $
    TL.fromStrict $ TE.decodeUtf8 $
    BL.toStrict $ WC.renderSetCookie cookie
```

## Reading an Encrypted Cookie

The middleware automatically decrypts cookies using the CSRF token:

```haskell
-- Middleware provides decrypted session
let fullApp =
      Wai.CSRF.middleware csrfConfig $ \mCsrfToken ->
        CC.middleware cryptoEnv handleSession mCsrfToken
  where
    handleSession (Just (csrfToken, Just session)) =
      -- Session is decrypted and available!
      -- Use session.userId, session.username, etc.
      app
    handleSession (Just (csrfToken, Nothing)) =
      -- CSRF token present but no session
      app
    handleSession Nothing =
      -- No CSRF token (shouldn't happen with middleware)
      app
```

## Security Features

| Feature | Implementation | Benefit |
|---------|---------------|---------|
| Encryption | AES-256-GCM-SIV | Confidentiality + Authentication |
| AEAD | CSRF token as AAD | Binds session to CSRF token |
| Nonce-misuse | SIV mode | Safe even if nonce repeats |
| Cookie flags | HttpOnly, Secure, SameSite | XSS and CSRF protection |
| Stateless | No server storage | Horizontal scaling |

## Comparison: wai-cryptocookie vs Web.Scotty.Session

| Aspect | Web.Scotty.Session | wai-cryptocookie |
|--------|-------------------|------------------|
| Storage | Server-side (STM) | Client-side (encrypted) |
| Cookie content | Random session ID | Encrypted session data |
| Scalability | Requires shared state | Stateless |
| Encryption | None (ID only) | AES-256-GCM-SIV |
| CSRF integration | Separate | Built-in via AAD |
| Server invalidation | Yes | No (expiration only) |
| Data size limit | Unlimited | ~4KB (cookie limit) |

## Use Cases

### Choose wai-cryptocookie when:

- Running multiple server instances
- Need stateless, horizontally scalable architecture
- Session data is small (< 4KB)
- Maximum security is required
- Want to avoid shared session storage
- Microservices architecture

### Choose Web.Scotty.Session when:

- Need server-side session invalidation
- Session data is large
- Simpler setup is preferred
- Single-server or monolithic deployment
- Don't want to depend on external middleware

## API References

- [wai-cryptocookie 0.3 documentation](https://hackage.haskell.org/package/wai-cryptocookie-0.3/docs/Wai-CryptoCookie.html)
- [wai-csrf 0.1 documentation](https://hackage.haskell.org/package/wai-csrf-0.1/docs/Wai-CSRF.html)
- [wai-cryptocookie source](https://github.com/k0001/hs-wai-cryptocookie)
- [Web.Scotty.Session (alternative)](https://hackage.haskell.org/package/scotty/docs/Web-Scotty-Session.html)

---

*This example demonstrates the complete integration pattern for wai-cryptocookie 0.3 and wai-csrf 0.1 with Scotty. The middleware is properly composed and encryption keys are managed securely. For production use, load keys from secure storage and enable HTTPS.*
