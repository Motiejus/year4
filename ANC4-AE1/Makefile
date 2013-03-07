CC=clang
CFLAGS=-Wall -Wextra -pedantic -g

OBJS=main.o msg_queue.o diagnostics.o

.PHONY: all clean

all: main

main: $(OBJS)

diagnostics.o main.o: diagnostics.h

diagnostics.o main.o msg_queue.o: types.h msg_queue.h

clean:
	rm -f main $(OBJS)
