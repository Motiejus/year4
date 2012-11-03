.PHONY: all test
all: Main

test:
	./test.sh

Main: Main.hs
	ghc Main.hs
