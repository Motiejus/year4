#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <assert.h>

#include "types.h"
#include "diagnostics.h"
#include "msg_queue.h"


/* GNU getline (necessary for mingw) */
ssize_t getline_g (char **lineptr, size_t *n, FILE *stream);

/* Number of nodes */
int N,
    tick = 0,
    split_horizon = 1,
    neighbour[MAX_NODES][MAX_NODES];

table_t routing_table[MAX_NODES];

/* Shortest path from -> to */
shortest_t shortest[MAX_NODES];
msg_q *q;


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
        shortest[node_from][node_to].cost = cost;
        shortest[node_to][node_from].cost = cost;
        shortest[node_from][node_to].via = node_to;
        shortest[node_to][node_from].via = node_from;

        neighbour[node_to][node_from] = neighbour[node_from][node_to] = 1;
        /* printf("From: %d, To: %d, Cost: %d\n", node_from, node_to, cost); */
    }
    /* # of nodes = highest numbered node + 1 */
    N += 1;

    free(buf);
    fclose(f);
}

/* Send shortest paths of self to all neighbours */
void
broadcast(int self) {
    int to;
    for (to = 0; to < N; to++) {
        if (neighbour[self][to])
            new_msg(q, tick+1, self, to, shortest[self]);
    }
}

void
receive(int self, int msg_from, shortest_t msg_tab) {
    int to, shortest_changed = 0;

    /* Cost from sender to self */
    cost_t cost = routing_table[self][msg_from][msg_from];

    for (to = 0; to < N; to++) {
        int via;
        cost_t old_shortest_self_to = shortest[self][to].cost;

        if (to == self || to == msg_from) continue;

        if (split_horizon && msg_tab[to].via == self)
            continue;

        routing_table[self][to][msg_from] = msg_tab[to].cost + cost;

        /* Recalculate shortest route to "to" */
        shortest[self][to].cost = routing_table[self][to][0];
        shortest[self][to].via = 0;
        for (via = 1; via < N; via++)
            if (routing_table[self][to][via] < shortest[self][to].cost) {
                shortest[self][to].cost = routing_table[self][to][via];
                shortest[self][to].via = via;
            }

        if (shortest[self][to].cost != old_shortest_self_to)
            shortest_changed = 1;
    }
    if (shortest_changed)
        broadcast(self);
}


/*
 * Iterate through all nodes' "inboxes" and do the job
 */
void
iterate() {
    /* On tick 0 do a broadcast */
    int i, got_smth = 1;
    msg_t *msg;

    for (i = 0; i < N; i++)
        broadcast(i);

    while (got_smth == 1) {
        diagnostics(q, tick, N, routing_table);
        tick++;
        got_smth = 0;
        while ((msg = pop_msg(q, tick)) != NULL) {
            got_smth = 1;
            receive(msg->to, msg->from, msg->table);
            destroy_msg(msg);
        }
    }
}

void preset() {
    int i,j,k;
    memset(neighbour, 0, sizeof(neighbour));

    for (i = 0; i < MAX_NODES; i++)
        for (j = 0; j < MAX_NODES; j++) {
            shortest[i][j].cost = MAX_DISTANCE;
            for (k = 0; k < MAX_NODES; k++)
                routing_table[i][j][k] = MAX_DISTANCE;
        }
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
    msg_q_destroy(q);

    return 0;
}