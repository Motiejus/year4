#ifndef __WSN4_TWEETER_H
#define __WSN4_TWEETER_H

#include <stdlib.h>

void post_tweet(int fd, int src, int dst, char *text, size_t length);

#endif /* __WSN4_TWEETER_H */
