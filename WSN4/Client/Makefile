# libmote.a in serial forwarder SDK dir must be precompiled. See README.

TINYBLOGMSG_GEN = TinyBlogMsg_gen.h TinyBlogMsg_gen.c

SFDIR ?= $(TOSROOT)/support/sdk/c/sf
CFLAGS += -Wall -Wextra -g
CFLAGS += -I$(SFDIR)
LIBMOTE_A ?= $(SFDIR)/libmote.a

main: main.o TinyBlogMsg_gen.o $(LIBMOTE_A)

main.o: TinyBlogMsg_gen.h

TinyBlogMsg_gen.o: $(TINYBLOGMSG_GEN)

$(TINYBLOGMSG_GEN): TinyBlogMsg.h
	mig c TinyBlogMsg.h TinyBlogMsg -o $@

clean:
	rm -f $(TINYBLOGMSG_GEN) *.o TinyBlogMsg.c main
.PHONY: clean
