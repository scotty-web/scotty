# Investigation: wai-cryptocookie for Session Management

This document provides an investigation into using `wai-cryptocookie` for session management in Scotty applications, as requested in issue #317.

## Overview

`wai-cryptocookie` is a Haskell library that provides encrypted cookies for WAI applications. It was authored by Renzo Carbonara (@k0001) and provides significant security improvements over basic session implementations.

**Package Information:**
- **Name:** wai-cryptocookie
- **Latest Version:** 0.3
- **Homepage:** https://github.com/k0001/hs-wai-cryptocookie
- **Hackage:** https://hackage.haskell.org/package/wai-cryptocookie

## Key Features

### Security Improvements

1. **Encrypted Cookie Values** - Cookie values are encrypted, not just signed
2. **Authenticated Encryption** - Uses AES-GCM for authenticated encryption
3. **Tampering Protection** - Prevents both reading and tampering of session data
4. **Proper Cookie Attributes** - Supports HttpOnly, Secure, and SameSite flags
5. **Key Management** - Built-in key derivation and rotation support
6. **Stateless Sessions** - No server-side storage required

## Comparison with Web.Scotty.Session

| Feature | Web.Scotty.Session | wai-cryptocookie |
|---------|-------------------|------------------|
| Storage | Server-side (STM/TVar) | Client-side (encrypted cookie) |
| Cookie Content | Random session ID (visible) | Encrypted data (opaque) |
| Tampering Protection | Via session ID lookup | Authenticated encryption |
| Confidentiality | Session ID visible | Full encryption |
| Scalability | Requires shared state | Stateless (no server storage) |
| Session Invalidation | Delete from server | Cookie expiration |
| Session Fixation | Requires ID regeneration | Built-in protection |

## When to Use Each

### Use wai-cryptocookie when:
- You need stateless session management
- You're running multiple server instances without shared state
- You want to avoid server-side session storage
- Session data is small and suitable for cookies (< 4KB)
- You need maximum security for session data
- You want to scale horizontally easily

### Use Web.Scotty.Session when:
- You need server-side session invalidation
- Session data is large (> 4KB)
- You want simpler setup without middleware
- You're running a single-server application
- You need to store large amounts of session data

## Integration Pattern

### 1. Add Dependency

Add to your `.cabal` file:

```cabal
build-depends: wai-cryptocookie >= 0.3
```

### 2. Initialize CryptoCookie Settings

```haskell
import qualified Wai.CryptoCookie as CC
import Data.ByteString (ByteString)

initCryptoCookie :: IO (CC.Settings, CC.State)
initCryptoCookie = do
    -- Load key from environment in production
    -- Key must be 32 bytes for AES-256
    key <- getEnv "SESSION_SECRET_KEY"
    
    let settings = CC.defaultSettings
            { CC.setCookieName = "session"
            , CC.setSecretKey = CC.secretKeyFromByteString key
            , CC.setMaxAge = Just 3600  -- 1 hour
            , CC.setHttpOnly = True     -- Prevent JavaScript access
            , CC.setSecure = True       -- Requires HTTPS
            , CC.setSameSite = CC.SameSiteLax  -- CSRF protection
            }
    
    state <- CC.mkState settings
    return (settings, state)
```

### 3. Apply Middleware to Scotty App

```haskell
import Network.Wai.Handler.Warp (run)
import Web.Scotty (scottyApp)

main :: IO ()
main = do
    (ccSettings, ccState) <- initCryptoCookie
    
    -- Create Scotty application
    app <- scottyApp $ do
        get "/" $ do
            text "Hello!"
    
    -- Wrap with cryptocookie middleware
    let appWithCrypto = CC.middleware ccSettings ccState app
    
    -- Run with Warp
    run 3000 appWithCrypto
```

### 4. Access Session Data in Actions

```haskell
import Network.Wai (vault)
import qualified Wai.CryptoCookie as CC

get "/profile" $ do
    req <- request  -- Get WAI request
    let v = vault req
    case CC.vaultLookup v of
        Nothing -> text "Not logged in"
        Just sessionData -> text "Welcome back!"
```

### 5. Set Session Data

```haskell
-- Setting session data requires access to the CryptoCookie state
-- This is typically done by storing it in application state
-- and passing it through to actions that need it
```

## Example Application

A complete example demonstrating the integration pattern is available in `examples/cryptocookie.hs`. Run it with:

```bash
cabal run scotty-cryptocookie
```

Then visit http://localhost:3000/ to see the documentation and integration guide.

## Security Considerations

### Production Deployment

1. **Key Management**
   - Load encryption keys from environment variables or secure key management services
   - Use a cryptographically secure random key (32 bytes for AES-256)
   - Implement key rotation procedures

2. **HTTPS Only**
   - Always set `setSecure = True` when using HTTPS
   - Never use without HTTPS in production

3. **Cookie Size**
   - Keep session data small (< 4KB recommended)
   - Large cookies impact performance and may be rejected by browsers

4. **Expiration**
   - Set appropriate `setMaxAge` based on your security requirements
   - Consider implementing automatic session renewal

## Implementation Notes

### Advantages
- No server-side state management
- Horizontally scalable by design
- No session database or shared storage needed
- Strong encryption protects session data
- Simpler deployment architecture

### Disadvantages
- Cannot invalidate sessions server-side
- Cookie size limits amount of session data
- Requires proper key management
- Slightly higher computational overhead per request

## Related Packages

- **wai-csrf** - CSRF protection middleware (complementary)
- **Web.Scotty.Session** - Server-side session management (alternative)
- **Web.Scotty.Cookie** - Basic cookie utilities (lower level)

## References

- [wai-cryptocookie on Hackage](https://hackage.haskell.org/package/wai-cryptocookie)
- [GitHub Repository](https://github.com/k0001/hs-wai-cryptocookie)
- [Web.Scotty.Session Documentation](https://hackage.haskell.org/package/scotty/docs/Web-Scotty-Session.html)
- [Issue #317](https://github.com/scotty-web/scotty/issues/317)

## Conclusion

`wai-cryptocookie` provides a secure, stateless alternative to traditional session management. It's particularly well-suited for:
- Microservices architectures
- Horizontally scaled applications
- Scenarios where server-side state is undesirable

For applications requiring server-side session invalidation or storing large amounts of session data, `Web.Scotty.Session` remains a viable alternative.

Both approaches have their place, and the choice depends on your specific requirements, deployment architecture, and security considerations.
