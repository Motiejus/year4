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
#include <serialpacket.h>

#include "tweeter.h"
#include "TinyBlogMsg_gen.h"
#include "TinyBlogMsgConsts.h"

int
post_tweet(int fd, int src, int dst, const char *text, size_t length) {
    size_t i;
    void *storage, *serialMsg_start, *blogMsg_start;
    tmsg_t *blogMsg, *serialMsg;

    /* Packet structure:
     * 00 (1 byte)
     * spacket (7 bytes)
     * payload (26 bytes)
     */
    uint8_t len = TINYBLOGMSG_SIZE + SPACKET_SIZE + sizeof(uint8_t);
    if ((storage = malloc(len)) == NULL) {
        perror("malloc"); exit(1);
    }
    ((uint8_t*)storage)[0] = 0x00;

    serialMsg_start = storage + sizeof(uint8_t);
    if ((serialMsg = new_tmsg(serialMsg_start, SPACKET_SIZE)) == NULL) {
        fprintf(stderr, "new_tmsg failure\n"); exit(1);
    }

    blogMsg_start = serialMsg_start + SPACKET_SIZE;
    if ((blogMsg = new_tmsg(blogMsg_start, TINYBLOGMSG_SIZE)) == NULL) {
        fprintf(stderr, "new_tmsg failure\n"); exit(1);
    }
    length = length > DATA_SIZE ? DATA_SIZE : length;

    spacket_header_dest_set(serialMsg, dst);
    spacket_header_src_set(serialMsg, src);
    spacket_header_length_set(serialMsg, TINYBLOGMSG_SIZE);
    spacket_header_type_set(serialMsg, TINYBLOGMSG_AM_TYPE);

    TinyBlogMsg_seqno_set(blogMsg, 0);
    TinyBlogMsg_sourceMoteID_set(blogMsg, src);
#if SCEN == 1
    TinyBlogMsg_destMoteID_set(blogMsg, 0);
    (void) dst;
#else
    TinyBlogMsg_destMoteID_set(blogMsg, dst);
#endif
    TinyBlogMsg_action_set(blogMsg, POST_TWEET);
    TinyBlogMsg_hopCount_set(blogMsg, 0);
    TinyBlogMsg_nchars_set(blogMsg, length);
    for (i = 0; i < DATA_SIZE; i++)
        TinyBlogMsg_data_set(blogMsg, i, i < length ? text[i] : '\0');
    TinyBlogMsg_mood_set(blogMsg, 0);

    if (write_sf_packet(fd, storage, len) == -1) {
        fprintf(stderr, "POST_TWEET write failed\n");
    }
    printf("Packet in hex: ");
    for (i = 0; i < len; i++) {
        printf("%02X ", ((uint8_t*)storage)[i]);
    }
    printf("\n");
    free_tmsg(blogMsg);
    free_tmsg(serialMsg);
    free(storage);
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
    TinyBlogMsg_nchars_set(msg, 0);
    for (i = 0; i < DATA_SIZE; i++)
        TinyBlogMsg_data_set(msg, i, '\0');
    TinyBlogMsg_mood_set(msg, 0);

    /*
    if (write_sf_packet(fd, tmsg_data(msg), TINYBLOGMSG_SIZE) == -1) {
        fprintf(stderr, "GET_TWEETS packet write failed\n");
    }
    */
    free_tmsg(msg);
    free(blogmsg);
}

void
get_a_tweet(int fd, int host_moteid, int user_moteid,
        size_t *text_len, char **text) {
    uint8_t *packet;
    int tmsg_len;
    tmsg_t *tmsg;

    if ((packet = read_sf_packet(fd, &tmsg_len)) == NULL) {
        printf("Error reading packet from sf\n"); exit(1);
    }
    tmsg = new_tmsg(packet, tmsg_len);
    *text_len = TinyBlogMsg_nchars_get(tmsg);

    printf("Received tweet msglen=%d, src=%d, dst=%d, nchars=%d. ",
            tmsg_len,
            TinyBlogMsg_sourceMoteID_get(tmsg),
            TinyBlogMsg_destMoteID_get(tmsg),
            *text_len);

    if (GET_TWEETS == TinyBlogMsg_action_get(tmsg) &&
            host_moteid == TinyBlogMsg_destMoteID_get(tmsg) &&
            user_moteid == TinyBlogMsg_sourceMoteID_get(tmsg)) {
        printf("Passing on\n");
        if ((*text = malloc(*text_len)) == NULL) {
            perror("malloc"); exit(1);
        }
        memcpy(*text, packet + TinyBlogMsg_data_offset(0), *text_len);
    } else {
        printf("Dropping\n");
        *text = NULL;
        *text_len = 0;
    }
    free(packet);
}

void
post_follow(int fd, int host_moteid, int user_moteid, int follow_who) {
    printf("Posting follow '%d'. fd: %d, host mote: %d, user mote: %d\n",
            follow_who, fd, host_moteid, user_moteid);
}
