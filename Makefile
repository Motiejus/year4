.PHONY: all
all: Main
	./Main

Main: Main.hs
	ghc Main.hs
