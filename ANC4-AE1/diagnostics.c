#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include "types.h"
#include "diagnostics.h"
#include "msg_queue.h"

void
routing_tables(int tick, int N, table_t *routing_table) {
    int self, via, to;

    printf("Routing tables (0 to %d) after tick %d:\n", N, tick);

    printf("Via  |");
    for (self = 0; self < N; self++) {
        for (via = 0; via < N; via++) {
            printf("%4d", via);
        }
        printf("  |");
    }
    printf("\n");

    printf("-----+");
    for (self = 0; self < N; self++) {
        for (via = 0; via < N; via++) {
            printf("----");
        }
        printf("--+");
    }

    printf("\n");
    for (to = 0; to < N; to++) {
        printf("To %d |", to);
        for (self = 0; self < N; self++) {
            for (via = 0; via < N; via++) {
                if (routing_table[self][to][via] < MAX_DISTANCE)
                    printf("%4d", routing_table[self][to][via]);
                else
                    printf("   .");
            }
            printf("  |");
        }
        printf("\n");
    }
    printf("\n");
}

/* Print best route between two points. */
void
best_route(int from, int to, shortest_t shortest[MAX_NODES]) {
    if (shortest[from][to].cost < MAX_DISTANCE) {
        int i;
        printf("Shortest route between %d and %d is: %d. ", from, to, shortest[from][to].cost);
        printf("Path: ");
        for (i = from; i != to; i = shortest[i][to].via) {
            printf("%d -> ", i);
        }
        printf("%d\n", i);
    }
    else {
        printf("There is no route between %d and %d.\n", from, to);
    }
}
