#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

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
    assert(q->head == NULL);
    assert(q->tail == NULL);
    free(q);
}

void
new_msg(msg_q *q, int tick, int from, int to, table_t *table) {
    msg_t *msg;
    if ((msg = malloc(sizeof(msg))) == NULL) {
        perror("malloc");
        exit(1);
    }
    msg->tick = tick;
    msg->from = from;
    msg->to = to;
    if ((msg->table = malloc(sizeof(table_t))) == NULL) {
        perror("malloc");
        exit(1);
    }
    memcpy(msg->table, table, sizeof(table_t));

    msg->next = NULL;
    if (q->head != NULL) {
        q->head->next = msg;
    }
    q->head = msg;
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
    free(msg->table);
    free(msg);
    msg = NULL;
}
