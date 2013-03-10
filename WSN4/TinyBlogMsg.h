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
#define TINY_BLOG_H

#include "TinyBlogMsgConsts.h"

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

#endif /* TINY_BLOG_H */
