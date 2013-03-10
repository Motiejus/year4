/*
 * if SCEN==1, POST_TWEET is a broadcast.
 * elseif SCEN==2, POST_TWEET is a message to user's mote.
 *
 * GET_TWEETS is always from host to user's mote.
 * Tweets themselves are retrieved from user's mote regardless of scenario.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <sfsource.h>
#include <message.h>

#include "tweeter.h"
#include "TinyBlogMsg_gen.h"
#include "TinyBlogMsgConsts.h"

int
post_tweet(int fd, int src, int dst, const char *text, size_t length) {
    size_t i;
    void *blogmsg;
    tmsg_t *msg;

    if ((blogmsg = malloc(TINYBLOGMSG_SIZE)) == NULL) {
        perror("malloc"); exit(1);
    }
    if ((msg = new_tmsg(blogmsg, TINYBLOGMSG_SIZE)) == NULL) {
        fprintf(stderr, "new_tmsg failure\n"); exit(1);
    }

    length = length > DATA_SIZE ? DATA_SIZE : length;

    TinyBlogMsg_seqno_set(msg, 0);
    TinyBlogMsg_sourceMoteID_set(msg, src);
#if SCEN == 1
    TinyBlogMsg_destMoteID_set(msg, 0);
    (void) dst;
#else
    TinyBlogMsg_destMoteID_set(msg, dst);
#endif
    TinyBlogMsg_action_set(msg, POST_TWEET);
    TinyBlogMsg_hopCount_set(msg, 0);
    TinyBlogMsg_nchars_set(msg, length);
    for (i = 0; i < DATA_SIZE; i++)
        TinyBlogMsg_data_set(msg, i, i < length ? text[i] : '\0');
    TinyBlogMsg_mood_set(msg, 0);

    if (write_sf_packet(fd, tmsg_data(msg), TINYBLOGMSG_SIZE) == -1) {
        fprintf(stderr, "Packet write failed\n");
    }
    free_tmsg(msg);
    free(blogmsg);
    return DATA_SIZE;
}

void
post_get_tweets(int fd, int host_moteid, int user_moteid) {
    int i;
    void *blogmsg;
    tmsg_t *msg;

    if ((blogmsg = malloc(TINYBLOGMSG_SIZE)) == NULL) {
        perror("malloc"); exit(1);
    }
    if ((msg = new_tmsg(blogmsg, TINYBLOGMSG_SIZE)) == NULL) {
        fprintf(stderr, "new_tmsg failure\n"); exit(1);
    }
    TinyBlogMsg_seqno_set(msg, 0);
    TinyBlogMsg_sourceMoteID_set(msg, host_moteid);
    TinyBlogMsg_destMoteID_set(msg, user_moteid);
    TinyBlogMsg_action_set(msg, GET_TWEETS);
    TinyBlogMsg_hopCount_set(msg, 0);
    for (i = 0; i < DATA_SIZE; i++)
        TinyBlogMsg_data_set(msg, i, '\0');
    TinyBlogMsg_mood_set(msg, 0);

    if (write_sf_packet(fd, tmsg_data(msg), TINYBLOGMSG_SIZE) == -1) {
        fprintf(stderr, "Packet write failed\n");
    }
    free_tmsg(msg);
    free(blogmsg);
}

char
**get_tweets(int fd, int host_moteid, int user_moteid) {
    printf("Getting tweets. fd: %d, host mote: %d, user mote: %d\n",
            fd, host_moteid, user_moteid);
    return NULL;
}

void
post_follow(int fd, int host_moteid, int user_moteid, int follow_who) {
    printf("Posting follow '%d'. fd: %d, host mote: %d, user mote: %d\n",
            follow_who, fd, host_moteid, user_moteid);
}
