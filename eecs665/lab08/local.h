#ifndef LOCAL_H
#define LOCAL_H

typedef struct local_t {
    struct local_t *next;
    long long size;
    long long offset;
} local_t;

extern local_t* local_init(long long size, long long offset, local_t *next);
extern void local_destroy(local_t *local);
extern void local_dump(local_t *local);
extern void local_print(local_t *local);

#endif
