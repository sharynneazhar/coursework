#include <stdio.h>
#include <stdlib.h>
#include <local.h>

local_t* local_init(long long size, long long offset, local_t *next) {
    local_t *local = (local_t*)malloc(sizeof(local_t));
    if (local == NULL) {
        perror("cannot allocate memory for formal localument");
        exit(1);
    }

    local->size = size;
    local->offset = offset;
    local->next = next;
    return local;
}

void local_destroy(local_t *local) {
    free(local);
}

void local_dump(local_t *local) {
    printf( "LOC: SIZE=%lld OFFSET=%lld\n", local->size, local->offset );
}

void local_print(local_t *local) {
    printf( "%lld(%%esp)", local->offset );
}
