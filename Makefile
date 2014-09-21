all: install run

install:
	cabal install --ghc-options=-Wall

run:
	cabal exec HOpenGL
