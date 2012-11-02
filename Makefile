.PHONY: all test
all: Main Main2

test: Main
	./Main

Main: Main.hs
	ghc Main.hs

Main2: Main2.hs
	ghc Main2.hs
