#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <message.h>
#include <sfsource.h>

#include "TinyBlogMsg_gen.h"
#include "TinyBlogMsgConsts.h"

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

int
main(int argc, char **argv) {
    int moteid, /* owner's mote id */
        fd; /* To serial forwarder */
    if (argc != 4) {
        fprintf(stderr, "Usage: %s host port MOTEID\n", argv[0]);
        exit(1);
    }
    fd = open_sf_source(argv[1], atoi(argv[2]));
    if (fd < 0) {
        fprintf(stderr, "Couldn't open serial forwarder at %s:%s\n",
                argv[1], argv[2]);
        exit(1);
    }
    moteid = atoi(argv[3]);
    if (moteid < 1 || moteid > 999) {
        fprintf(stderr, "MOTEID must be between 1 and 999, given %d\n", moteid);
        exit(1);
    }

    post_tweet(fd, moteid, 1, "labas\04", 6);

    return 0;
}
