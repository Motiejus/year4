#ifndef __ANC4_TYPES_H
#define __ANC4_TYPES_H 1

#define MAX_NODES 4
#define MAX_DISTANCE 999 /* must be < 2^32 - 2 */
typedef int cost_t;

/*
 * 1. owner (simulating distributed environment)
 * 2. to
 * 3. via
 */
typedef cost_t table_t[MAX_NODES][MAX_NODES];
typedef cost_t shortest_t[MAX_NODES];

#endif /* __ANC4_TYPES_H */
