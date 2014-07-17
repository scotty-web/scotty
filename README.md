# Scotty [![Build Status](https://travis-ci.org/scotty-web/scotty.svg)](https://travis-ci.org/scotty-web/scotty)[![Coverage Status](https://coveralls.io/repos/scotty-web/scotty/badge.png?branch=master)](https://coveralls.io/r/scotty-web/scotty?branch=master)

A Haskell web framework inspired by Ruby's Sinatra, using WAI and Warp.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty

import Data.Monoid (mconcat)

main = scotty 3000 $ do
    get "/:word" $ do
        beam <- param "word"
        html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
```

Scotty is the cheap and cheerful way to write RESTful, declarative web applications.

* A page is as simple as defining the verb, url pattern, and Text content.
* It is template-language agnostic. Anything that returns a Text value will do.
* Conforms to WAI Application interface.
* Uses very fast Warp webserver by default.

See examples/basic.hs to see Scotty in action. (basic.hs needs the wai-extra package)

```bash
> runghc examples/basic.hs
Setting phasers to stun... (port 3000) (ctrl-c to quit)
(visit localhost:3000/somepath)
```

As for the name: Sinatra + Warp = Scotty.

### More Information

Tutorials and related projects can be found in the Scotty wiki:

https://github.com/scotty-web/scotty/wiki

### Development & Support

Open an issue on GitHub or join `#scotty` on Freenode.

Copyright (c) 2012-2014 Andrew Farmer
