#ifndef __WSN4_TWEETER_H
#define __WSN4_TWEETER_H

#include <stdlib.h>

int post_tweet(int fd, int src, int dst, const char *text, size_t length);
void post_follow(int fd, int host_moteid, int user_moteid, int follow_who);
void post_get_tweets(int fd, int host_moteid, int user_moteid);
void get_a_tweet(int fd, int host_moteid, int user_moteid,
        size_t *len, char **msg);

#endif /* __WSN4_TWEETER_H */
