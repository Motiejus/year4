CC=clang
CFLAGS=-Wall -Wextra -pedantic -g

.PHONY: all clean

all: main

main: main.o msg_queue.o diagnostics.o getdelim.o

diagnostics.o main.o: diagnostics.h

diagnostics.o main.o msg_queue.o: types.h msg_queue.h

clean:
	rm -f main main.o msg_queue.o diagnostics.o
