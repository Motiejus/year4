#ifndef __ANC4_DIAGNOSTICS_h
#define __ANC4_DIAGNOSTICS_h

#include "msg_queue.h"

void routing_tables(int tick, int N, table_t *routing_table);
void best_route(int from, int to, shortest_t shortest[MAX_NODES]);

#endif
