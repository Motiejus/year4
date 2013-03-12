# libmote.a in serial forwarder SDK dir must be precompiled. See README.

SCEN ?= 1

CFLAGS += -DSCEN=$(SCEN)

.PHONY: all clean

TinyBlogMsg.py: TinyBlogMsg.h
	mig -python-classname=TinyBlogMsg python TinyBlogMsg.h TinyBlogMsg -o $@

clean:
	rm -f TinyBlogMsg.py *.pyc
