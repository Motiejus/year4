.PHONY: all test
all: Main

test:
	./Main | grep -v Usage
	./test.sh

Main: Main.hs
	ghc Main.hs

clean:
	rm -f Main *.hi *.o
