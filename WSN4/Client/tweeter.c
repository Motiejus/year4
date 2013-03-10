#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <sfsource.h>
#include <message.h>

#include "TinyBlogMsg_gen.h"
#include "TinyBlogMsgConsts.h"

#include "tweeter.h"

void
post_tweet(int fd, int src, int dst, char *text, size_t length) {
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
    TinyBlogMsg_destMoteID_set(msg, dst);
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
