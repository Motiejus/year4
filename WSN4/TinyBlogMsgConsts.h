#ifndef __TINY_BLOG_MSG_CONSTS
#define __TINY_BLOG_MSG_CONSTS

/*
 * This file has constants moved from TinyBlogMsg.h, so can be
 * included separately
 */

/* This number is arbitrary */
enum {
  AM_TINYBLOGMSG = 10
};

/* Number of bytes per message. If you increase this, you will have to increase the message_t size,
   by setting the macro TOSH_DATA_LENGTH
   See $TOSROOT/tos/types/message.h
 */
enum {
  DATA_SIZE = 14
};

/* Actions 
 Here you can add additional actions
 */
enum {
  POST_TWEET = 1,
  ADD_USER = 2,
  GET_TWEETS
};

#endif /* __TINY_BLOG_MSG_CONSTS */
