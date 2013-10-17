boot:
	cabal install --force-reinstalls

ghci:
	ghc --interactive -Wall Web/Scotty/Helpers.hs
