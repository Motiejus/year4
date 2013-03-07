CFLAGS=-Wall -Wextra -pedantic -g

OBJS=main.o msgqueue.o diagnostics.o

.PHONY: all clean

all: main

main: $(OBJS)

diagnostics.o main.o: diagnostics.h

diagnostics.o main.o msgqueue.o: types.h msgqueue.h

clean:
	rm -f main $(OBJS)
