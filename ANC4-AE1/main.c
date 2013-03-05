#define _GNU_SOURCE
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <assert.h>

#include "types.h"
#include "diagnostics.h"
#include "msg_queue.h"


ssize_t getline_g (char **lineptr, size_t *n, FILE *stream);

/* Number of nodes */
int N;
table_t routing_table[MAX_NODES];
msg_q *q;
int tick = 0;

int neighbor[MAX_NODES][MAX_NODES];

void
read_data(const char *filename) {
    FILE *f;
    size_t n;
    int node_from, node_to, eof;
    cost_t cost;
    char *buf = NULL;

    if ((f = fopen(filename, "r")) == NULL) { perror("fopen"); exit(1); }

    /* Ignore the first line (set defn). We make an assumption that
       all node names are integers and they are strictly increasing by 1 */
    if ((n = getline_g(&buf, &n, f)) <= 0) { perror("getline_g"); exit(1); }

    if (getline_g(&buf, &n, f) <= 0) { perror("getline_g"); exit(1); }
    for (eof = 1; eof > 0; eof = getline_g(&buf, &n, f)) {
        assert(sscanf(buf, "(%d,%d,%d)", &node_from, &node_to, &cost) == 3);
        N = N < node_from? node_from : N;
        N = N < node_to? node_to : N;
        routing_table[node_from][node_to][node_to] = cost;
        routing_table[node_to][node_from][node_from] = cost;
        neighbor[node_to][node_from] = neighbor[node_from][node_to] = 1;
        /* printf("From: %d, To: %d, Cost: %d\n", node_from, node_to, cost); */
    }
    /* # of nodes = highest numbered node + 1 */
    N += 1;

    free(buf);
    fclose(f);
}

/* Send self routing table to all neighbours */
void
broadcast(int self) {
    int to;
    for (to = 0; to < N; to++) {
        if (neighbor[self][to])
            new_msg(q, tick, self, to, &routing_table[self]);
    }
}

void
receive(int self, int msg_from, table_t msg_tab) {
    int to, via;
    for (to = 0; to < N; to++) {
        for (via = 0; via < N; via++) {
            cost_t cost = msg_tab[self][self];
            if (routing_table[self][msg_from][to] > msg_tab[via][to] + cost) {
                routing_table[self][msg_from][to] = msg_tab[via][to] + cost;
                broadcast(self);
            }
        }
    }
}


/*
 * Iterate through all nodes' "inboxes" and do the job
 */
void
iterate() {
    /* On tick 0 do a broadcast */
    int got_smth = 1, i;
    msg_t *msg;

    for (i = 0; i < N; i++)
        broadcast(i);

    for (; got_smth == 1; tick++) {
        got_smth = 0;
        while ((msg = pop_msg(q, tick)) != NULL) {
            got_smth = 1;
            receive(msg->to, msg->from, *msg->table);
            destroy_msg(msg);
        }
        diagnostics(q, tick, N, routing_table);
    }
}

void preset() {
    int i,j,k;
    memset(neighbor, 0, sizeof(neighbor));

    for (i = 0; i < MAX_NODES; i++)
        for (j = 0; j < MAX_NODES; j++)
            for (k = 0; k < MAX_NODES; k++)
                routing_table[i][j][k] = MAX_DISTANCE;
}

int
main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s SAMPLE_FILE\n", argv[0]);
        exit(1);
    }
    q = msg_q_create();
    preset();
    read_data(argv[1]);
    iterate();

    return 0;
}
