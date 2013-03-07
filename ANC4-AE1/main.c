#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <assert.h>

#include "types.h"
#include "diagnostics.h"
#include "msg_queue.h"

#define MAX_LINE 255

/* Number of nodes */
int N,
    tick = 0,
    split_horizon = 0;

table_t routing_table[MAX_NODES];

/* Shortest path from -> to */
shortest_t shortest[MAX_NODES];
msg_q *q;


void
read_data(const char *filename) {
    FILE *f;
    int node_from, node_to;
    cost_t cost;
    char buf[MAX_LINE];

    if ((f = fopen(filename, "r")) == NULL) { perror("fopen"); exit(1); }

    /* Ignore the first line (set defn). We make an assumption that
       all node names are integers and they are strictly increasing by 1 */
    if (fgets(buf, MAX_LINE, f) == NULL) { perror("fgets"); exit(1); }

    while (fgets(buf, MAX_LINE, f) != NULL) {
        assert(sscanf(buf, "(%d,%d,%d)", &node_from, &node_to, &cost) == 3);
        N = N < node_from? node_from : N;
        N = N < node_to? node_to : N;
        routing_table[node_from][node_to][node_to] = cost;
        routing_table[node_to][node_from][node_from] = cost;
        shortest[node_from][node_to].cost = cost;
        shortest[node_to][node_from].cost = cost;
        shortest[node_from][node_to].via = node_to;
        shortest[node_to][node_from].via = node_from;

        /* printf("From: %d, To: %d, Cost: %d\n", node_from, node_to, cost); */
    }
    /* # of nodes = highest numbered node + 1 */
    N += 1;

    fclose(f);
}

/* Send shortest paths of self to all neighbours */
void
broadcast(int self) {
    int to;
    for (to = 0; to < N; to++) {
        if (routing_table[self][to][to] < MAX_DISTANCE)
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

/* Update shortest routes of 'self' that go through b */
void
update_shortest(int self, int b) {
    int to, via, shortest_changed = 0;

    for (to = 0; to < N; to++) {
        if (to == self) continue;
        if (shortest[self][to].via == b) {
            shortest[self][to].cost = MAX_DISTANCE;
            shortest_changed = 1;
            /* Find other shortest path which does not go through b */
            for (via = 0; via < N; via++)
                if (routing_table[self][to][via] < shortest[self][to].cost) {
                    shortest[self][to].cost = routing_table[self][to][via];
                    shortest[self][to].via = via;
                }
        }
    }

    if (shortest_changed)
        broadcast(self);
}

/*
 * Modify route between 2 nodes
 */
void
modify_link(int a, int b, cost_t new_cost) {

    routing_table[a][b][b] = new_cost;
    routing_table[b][a][a] = new_cost;

    /* Recalculate a and b shortest paths that go via each other */
    update_shortest(a, b);
    update_shortest(b, a);
}

/*
 * Iterate through all nodes' "inboxes" and do the job. Tick by tick.
 * Return 1 if there is more work, 0 otherwise.
 */
int
iterate() {
    /* On tick 0 do a broadcast */
    int got_smth = 0;
    msg_t *msg;

    tick++;
    while ((msg = pop_msg(q, tick)) != NULL) {
        got_smth = 1;
        receive(msg->to, msg->from, msg->table);
        destroy_msg(msg);
    }
    return got_smth;
}

void preset() {
    int i,j,k;
    for (i = 0; i < MAX_NODES; i++)
        for (j = 0; j < MAX_NODES; j++) {
            shortest[i][j].cost = MAX_DISTANCE;
            for (k = 0; k < MAX_NODES; k++)
                routing_table[i][j][k] = MAX_DISTANCE;
        }
}

void
ui() {
    int cont = 1;
    char buf[MAX_LINE];

    while (cont) {
        int from, to;
        cost_t new_cost;

        printf("Enter your command (h for help):\n> ");
        fgets(buf, MAX_LINE, stdin);
        if (strrchr(buf, '\n') != NULL)
            *strrchr(buf, '\n') = '\0';

        if (strstr(buf, "n") == buf) { /* next */
            iterate();
            printf("Tick %d iteration complete. Split horizon = %d. ",
                    tick, split_horizon);
            if (peek_msg(q, tick+1) != NULL)
                printf("Not converged.\n");
            else
                printf("Route converged.\n");
        } else if (strstr(buf, "s") == buf) {
            cont = 0;
        } else if (tick == 0 && strstr(buf, "z") == buf) {
            split_horizon = !split_horizon;
            printf("Split horizon = %d\n", split_horizon);
        } else if (strstr(buf, "p") == buf) {
            routing_tables(tick, N, routing_table);
        } else if (sscanf(buf, "m %d %d %d", &from, &to, &new_cost) == 3) {
            modify_link(from, to, new_cost);
            printf("Link modified\n");
        } else if (sscanf(buf, "d %d %d", &from, &to) == 2) {
            modify_link(from, to, MAX_DISTANCE);
            printf("Link dropped\n");
        } else if (sscanf(buf, "b %d %d", &from, &to) == 2) {
            best_route(from, to, shortest);
        } else if (strstr(buf, "h") == buf) {
            printf("Commands\n");
            if (tick == 0)
                printf("  z                    - toggle split horizon\n");
            printf("  m [from] [to] [cost] - modify link\n"
                   "  d [from] [to]        - delete link\n"
                   "  n                    - next iteration\n"
                   "  b [from] [to]        - print best route\n"
                   "  p                    - print routing table\n"
                   "  s                    - stop\n"
                   "  h                    - this help\n"
                  );
        } else {
            printf("Unknown command '%s'. Press 'h' for help.\n", buf);
        }
    }
}

int
main(int argc, char **argv) {
    int i;
    if (argc != 2) {
        fprintf(stderr, "Usage: %s SAMPLE_FILE\n", argv[0]);
        exit(1);
    }
    q = msg_q_create();
    preset();
    read_data(argv[1]);

    for (i = 0; i < N; i++)
        broadcast(i);


    ui();
    msg_q_destroy(q);


    return 0;
}
