#ifndef TEMP_H
#define TEMP_H

#include <arg.h>
#include <local.h>

#define OPERAND_PARAM   0
#define OPERAND_LOCAL   1
#define OPERAND_GLOBAL  2
#define OPERAND_INT     3

typedef struct temp_t {
    struct temp_t *next;
    unsigned type;
    char *name;

    union {
        arg_t *arg;
        local_t *local;
        long long val;
        const char *glb;
    };
} temp_t;

#endif
#ifndef OPER_H
#define OPER_H

typedef struct oper_t {
} oper_t;

extern temp_t* temp_param(char *name, arg_t *arg, temp_t *next);
extern temp_t* temp_local(char *name, local_t *local, temp_t *next);
extern temp_t* temp_int(char *name, long long val, temp_t *next);
extern temp_t* temp_global(char *name, const char *glb, temp_t *next);
extern void temp_destroy(temp_t *tmp);
extern void temp_print(temp_t *tmp);
extern void temp_label(temp_t *tmp);

#endif
