.PHONY: all test
all: Main

test: Main
	./Main

Main: Main.hs
	ghc Main.hs
