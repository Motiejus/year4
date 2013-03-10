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

#include "tweeter.h"

#define MAX_LINE 255

void check_moteid(char *name, int moteid);


/*
void
get_tweets(int fd, int src) {

}

void
ui(int fd, int moteid) {
    char buf[MAX_LINE];
    int cont = 1;

    while(cont) {


    }
}
*/

int
main(int argc, char **argv) {
    int host_moteid, /* id of mote connected to this host */
        user_moteid, /* user's mote id */
        fd; /* To serial forwarder */
    if (argc != 5) {
        fprintf(stderr, "Usage: %s host port HOST_MOTEID USER_MOTEID\n",
                argv[0]);
        exit(1);
    }
    fd = open_sf_source(argv[1], atoi(argv[2]));
    if (fd < 0) {
        fprintf(stderr, "Couldn't open serial forwarder at %s:%s\n",
                argv[1], argv[2]);
        exit(1);
    }
    check_moteid("HOST", host_moteid = atoi(argv[3]));
    check_moteid("USER", user_moteid = atoi(argv[4]));

    //ui(fd, moteid);
    post_tweet(fd, host_moteid, user_moteid, "labas\04", 6);

    return 0;
}

void
check_moteid(char *name, int moteid) {
    if (moteid < 1 || moteid > 999) {
        fprintf(stderr, "%s_MOTEID must be between 1 and 999, given %d\n",
                name, moteid);
        exit(1);
    }
}
