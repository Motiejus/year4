#ifndef __ANC4_MSG_QUEUE_H
#define __ANC4_MSG_QUEUE_H 1

#include "types.h"

typedef struct msg_struct {
    int tick,
        from,
        to;
    table_t *table;
    struct msg_struct *next;
} msg_t;

typedef struct {
    msg_t *tail,
          *head;
} msg_q;


/* Create a new empty message queue */
msg_q* msg_q_create();

/* Destroy message queue. Must be empty */
void msg_q_destroy(msg_q *q);

/* Add message to node inbox */
void new_msg(msg_q *q, int tick, int from, int to, table_t *table);

/* Pop the next message for this tick.
 *
 * Returned pointer must be destroyed by destroy_msg(). */
msg_t* pop_msg(msg_q *q, int tick);

void destroy_msg(msg_t *msg);

#endif /* __ANC4_MSG_QUEUE_H */
