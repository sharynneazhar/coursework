#include <stdio.h>
#include <stdlib.h>
#include <arg.h>

arg_t* arg_init(long long size, long long offset, arg_t *next) {
    arg_t *arg = (arg_t*)malloc(sizeof(arg_t));
    if (arg == NULL) {
        perror("cannot allocate memory for formal argument");
        exit(1);
    }

    arg->size = size;
    arg->offset = offset;
    arg->next = next;
    return arg;
}

void arg_destroy(arg_t *arg) {
    free(arg);
}

void arg_dump(arg_t *arg) {
    printf( "ARG: SIZE=%lld OFFSET=%lld\n", arg->size, arg->offset );
}


void arg_print(arg_t *arg) {
    printf( "%lld(%%esp)", arg->offset );
}
