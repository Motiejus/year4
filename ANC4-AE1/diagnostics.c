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

    printf("Tick %d completed. Routing tables:\n", tick);

    for (self = 0; self < N; self++) {
        printf("Routing table for %d:\n", self);
        for (via = 0; via < N; via++) {
            for (to = 0; to < N; to++) {
                if (routing_table[self][via][to] < MAX_DISTANCE)
                    printf("%4d ", routing_table[self][via][to]);
                else
                    printf("  -  ");
            }
            printf("\n");
        }
        printf("\n\n");
    }
}
