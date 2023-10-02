# Scotty [![Hackage](http://img.shields.io/hackage/v/scotty.svg)](https://hackage.haskell.org/package/scotty) [![CI](https://github.com/scotty-web/scotty/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/scotty-web/scotty/actions/workflows/haskell-ci.yml)

A Haskell web framework inspired by Ruby's Sinatra, using WAI and Warp.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty

main = scotty 3000 $
    get "/:word" $ do
        beam <- captureParam "word"
        html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
```

Scotty is the cheap and cheerful way to write RESTful, declarative web applications.

* A page is as simple as defining the verb, URL pattern, and Text content.
* It is template-language agnostic. Anything that returns a Text value will do.
* Conforms to the [web application interface (WAI)](https://github.com/yesodweb/wai/).
* Uses the very fast Warp webserver by default.

See examples/basic.hs to see Scotty in action. (basic.hs needs the wai-extra package)

```bash
> runghc examples/basic.hs
Setting phasers to stun... (port 3000) (ctrl-c to quit)
(visit localhost:3000/somepath)
```

As for the name: Sinatra + Warp = Scotty.

## More Information

Tutorials and related projects can be found in the [Scotty wiki](https://github.com/scotty-web/scotty/wiki).

## Contributing

Feel free to ask questions or report bugs on the [Github issue tracker](https://github.com/scotty-web/scotty/issues/).

Github issues are now (September 2023) labeled, so newcomers to the Haskell language can start with `easy fix` ones and gradually progress to `new feature`s, `bug`s and `R&D` :)

## Package versions

Scotty adheres to the [Package Versioning Policy](https://pvp.haskell.org/).


## FAQ

* Fails to compile regex-posix on Windows
    * If you are using stack, add the following parameters to `stack.yaml`:
        * ```yaml
            extra-deps:
            - regex-posix-clib-2.7
            flags:
              regex-posix:
                _regex-posix-clib: true
          ```
    * If you are using cabal, update the `constraints` section of `cabal.project.local` as follows:
        * ```
          constraints:
            regex-posix +_regex-posix-clib 
          ```

### Contributors

<a href="https://github.com/scotty-web/scotty/graphs/contributors">
  <img src="https://contrib.rocks/image?repo=scotty-web/scotty" />
</a>


# Copyright 
(c) 2012-Present, Andrew Farmer and Scotty contributors
