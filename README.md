# Scotty [![Hackage](http://img.shields.io/hackage/v/scotty.svg)](https://hackage.haskell.org/package/scotty) [![Stackage Lts](http://stackage.org/package/scotty/badge/lts)](http://stackage.org/lts/package/scotty) [![Stackage Nightly](http://stackage.org/package/scotty/badge/nightly)](http://stackage.org/nightly/package/scotty) [![CI](https://github.com/scotty-web/scotty/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/scotty-web/scotty/actions/workflows/haskell-ci.yml)

A Haskell web framework inspired by Ruby's Sinatra, using WAI and Warp.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty

main = scotty 3000 $
    get "/:word" $ do
        beam <- pathParam "word"
        html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
```

Scotty is the cheap and cheerful way to write RESTful, declarative web applications.

* A page is as simple as defining the verb, URL pattern, and Text content.
* It is template-language agnostic. Anything that returns a Text value will do.
* Conforms to the [web application interface (WAI)](https://github.com/yesodweb/wai/).
* Uses the very fast Warp webserver by default.

As for the name: Sinatra + Warp = Scotty.

## Examples

Run `examples/basic.hs` to see Scotty in action (if you haven't, run `cabal update` first):

```bash
cabal run scotty-basic
```

`Setting phasers to stun... (port 3000) (ctrl-c to quit)`

Once the server is running you can interact with it with `curl` or a browser:

```bash
curl localhost:3000
```

`foobar`

```bash
curl localhost:3000/foo_query?p=42
```

`<h1>42</h1>`

Additionally, the `examples` directory shows a number of concrete use cases, e.g. 

* [exception handling](./examples/exceptions.hs)
* [global state](./examples/globalstate.hs)
* [configuration](./examples/reader.hs)
* [cookies](./examples/cookies.hs)
* [file upload](./examples/upload.hs)
* and more

You can run these using `cabal run scotty-exceptions`, `cabal run scotty-globalstate` etc.

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
