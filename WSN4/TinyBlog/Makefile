COMPONENT=TinyBlogAppC

BUILD_EXTRA_DEPS=TinyBlogMsg.py
CLEAN_EXTRA=$(BUILD_EXTRA_DEPS) *.pyc

TinyBlogMsg.py: TinyBlogMsg.h
	mig -python-classname=TinyBlogMsg python TinyBlogMsg.h TinyBlogMsg -o $@

include $(MAKERULES)
