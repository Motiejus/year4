#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>

#include <sfsource.h>

#include "tweeter.h"

#define MAX_LINE 255

void check_moteid(char *name, int moteid);
int get_from_ui_or_sf(int sf);

void
ui(int fd, int host_moteid, int user_moteid) {
    char buf[MAX_LINE];
    int cont = 1,
        print_help,
        follow_who;
    size_t length;

    while (cont) {
        int stream;
        if (print_help)
            printf("Enter your command or tweet (/h for help):\n> ");
        /* Print above message on next iteration */
        print_help = 1;

        stream = get_from_ui_or_sf(fd);
        if (stream == 1) { /* UI */
            fgets(buf, MAX_LINE - 2, stdin);
            length = strlen(buf);
            if (strchr(buf, '\n') != NULL) {
                *strchr(buf, '\n') = '\0';
                buf[length-1] = '\04'; /* replace '\0' by EOT */
            } else {
                buf[length++] = '\04';
                buf[length] = '\0';
                cont = 0;
            }
            if (strstr(buf, "/h") == buf) {
                printf("  /follow SOMEBODY - express interest in SOMEBODY\n"
                        "  /h               - this help\n"
                        "  /q               - quit\n"
                        "  ANYTHING ELSE    - tweet the message\n");
            } else if (strstr(buf, "/q") == buf) {
                cont = 0;
            } else if(sscanf(buf, "/follow %d", &follow_who) == 1) {
                post_follow(fd, host_moteid, user_moteid, follow_who);
            } else {
                int final_length = post_tweet(fd, host_moteid, user_moteid,
                        buf, length);
                buf[final_length-1] = '\0';
                printf("Tweeted: '%s' (see if red LED toggled)\n", buf);
            }
        }
        else if (stream == 2) {
            char *tweet;
            get_a_tweet(fd, host_moteid, user_moteid, &length, &tweet);
            printf("Got tweet of length %d: '", length);
            write(fileno(stdout), tweet, length);
            printf("'\n");
            free(tweet);
        } else if (stream == 0) {
            print_help = 0;
            post_get_tweets(fd, host_moteid, user_moteid);
        }
    }
}

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

    ui(fd, host_moteid, user_moteid);

    return 0;
}


/*
 * HELPERS
 */
void
check_moteid(char *name, int moteid) {
    if (moteid < 1 || moteid > 999) {
        fprintf(stderr, "%s_MOTEID must be between 1 and 999, given %d\n",
                name, moteid);
        exit(1);
    }
}

/*
 * Get data from UI or serial forwarder.
 * Return:
 * 0 if no data
 * 1 if UI
 * 2 if SF
 */
int
get_from_ui_or_sf(int sf) {
    fd_set rfds;
    struct timeval tv;
    int retval;

    /* Watch stdin and serial forwarder for input. */
    FD_ZERO(&rfds);
    FD_SET(fileno(stdin), &rfds);
    FD_SET(sf, &rfds);

    /* Wait up to five seconds */
    tv.tv_sec = 5;
    tv.tv_usec = 0;

    retval = select(1, &rfds, NULL, NULL, &tv);

    if (retval == -1) {
        perror("select");
        exit(1);
    } else if (retval) {
        if (FD_ISSET(fileno(stdin), &rfds))
            return 1;
        else if (FD_ISSET(sf, &rfds))
            return 2;
    }
    return 0;
}
