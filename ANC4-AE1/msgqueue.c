#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "types.h"
#include "msgqueue.h"

msg_q*
msg_q_create() {
    msg_q *q;
    if ((q = malloc(sizeof(msg_q))) == NULL) {
        perror("malloc");
        exit(1);
    }
    q->tail = q->head = NULL;
    return q;
}

void
msg_q_destroy(msg_q *q) {
    free(q);
}

void
new_msg(msg_q *q, int tick, int from, int to, shortest_t table) {
    int i;
    msg_t *msg;
    if ((msg = malloc(sizeof(msg_t))) == NULL) {
        perror("malloc");
        exit(1);
    }
    msg->tick = tick;
    msg->from = from;
    msg->to = to;

    for (i = 0; i < MAX_NODES; i++)
        msg->table[i] = table[i];

    msg->next = NULL;
    if (q->head != NULL) {
        q->head->next = msg;
    }
    q->head = msg;
    if (q->tail == NULL)
        q->tail = msg;
}

/* Is there anything in message queue? */
msg_t*
peek_msg(msg_q *q, int tick) {
    msg_t *msg = q->tail;
    if (msg == NULL)
        return NULL;
    if (msg->tick != tick)
        return NULL;
    return msg;
}
 
msg_t*
pop_msg(msg_q *q, int tick) {
    msg_t *msg = peek_msg(q, tick);
    if (msg == NULL)
        return NULL;
    q->tail = msg->next;
    return msg;
}

void
destroy_msg(msg_t *msg) {
    free(msg);
}
