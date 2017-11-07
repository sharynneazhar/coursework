#ifndef ARG_H
#define ARG_H

typedef struct arg_t {
    struct arg_t *next;
    long long size;
    long long offset;
} arg_t;

extern arg_t* arg_init(long long size, long long offset, arg_t *next);
extern void arg_destroy(arg_t *arg);
extern void arg_dump(arg_t *arg);
extern void arg_print(arg_t *arg);

#endif
