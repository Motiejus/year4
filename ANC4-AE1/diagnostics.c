#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include "types.h"
#include "diagnostics.h"
#include "msg_queue.h"

void
diagnostics(msg_q *q, int tick, int N, table_t *routing_table) {
    int self, via, to;
    (void) q;

    printf("Tick %d completed. Routing tables (0 to %d):\n", tick, N);

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
