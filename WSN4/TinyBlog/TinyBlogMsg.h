/** 
 * File Name: TinyBlog.h
 *
 * Description:
 * This header file defines the message
 * types for the TinyBlog application.
 * 
 * @author: Wim Vanderbauwhede 2012
 */
#ifndef TINY_BLOG_H
#define  TINY_BLOG_H

/* This number is arbitrary */
enum {
  AM_TINYBLOGMSG = 10
};

/* Number of bytes per message. If you increase this, you will have to increase the message_t size,
   by setting the macro TOSH_DATA_LENGTH
   See $TOSROOT/tos/types/message.h
 */
enum {
  DATA_SIZE = 100
};

#define TOSH_DATA_LENGTH 102

typedef nx_struct TinyBlogMsg {
    nx_uint8_t seqno;
    nx_uint16_t sourceMoteID;
    nx_uint16_t destMoteID;
    nx_uint8_t action; // see enum below
    nx_uint8_t hopCount;
    nx_uint8_t nchars;  
    nx_uint8_t data[DATA_SIZE];
    nx_uint32_t mood;
} tinyblog_t;

/* Actions 
 Here you can add additional actions
 */
enum {
  POST_TWEET = 1,
  ADD_USER = 2,
  GET_TWEETS
};

#endif
