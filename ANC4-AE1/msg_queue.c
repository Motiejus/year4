#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "types.h"
#include "msg_queue.h"

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
new_msg(msg_q *q, int tick, int from, int to, table_t table) {
    int i, j;
    msg_t *msg;
    if ((msg = malloc(sizeof(msg_t))) == NULL) {
        perror("malloc");
        exit(1);
    }
    msg->tick = tick;
    msg->from = from;
    msg->to = to;

    for (i = 0; i < MAX_NODES; i++)
        for (j = 0; j < MAX_NODES; j++)
            msg->table[i][j] = table[i][j];

    msg->next = NULL;
    if (q->head != NULL) {
        q->head->next = msg;
    }
    q->head = msg;
    if (q->tail == NULL)
        q->tail = msg;
}

msg_t*
pop_msg(msg_q *q, int tick) {
    msg_t *msg = q->tail;
    if (msg == NULL)
        return NULL;
    if (msg->tick != tick)
        return NULL;
    q->tail = msg->next;
    return msg;
}

void
destroy_msg(msg_t *msg) {
    free(msg);
}
