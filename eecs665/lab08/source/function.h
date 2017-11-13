#ifndef FUNCTION_H
#define FUNCTION_H

#include <arg.h>
#include <temp.h>
#include <local.h>
#include <strng.h>

#define STACK_SIZE 256

typedef struct function_t {
    char *name;
    arg_t *args;
    local_t *locals;
    temp_t *temps;
    string_t *strings;

    long long arg_offset;
    long long local_offset;
} function_t;

extern function_t* function;

extern function_t* function_init(char *name);
extern void function_destroy(function_t *func);
extern void function_dump(function_t *func);

extern arg_t* function_newarg(function_t *func, long long size);
extern local_t* function_newlocal(function_t *func, long long size);

extern temp_t* function_getarg(function_t *func, unsigned num, char *name);
extern temp_t* function_getlocal(function_t *func, unsigned num, char *name);
extern temp_t* function_gettemp(function_t *func, long long size, char *name);
extern temp_t* function_getint(function_t *func, long long val, char *name);
extern temp_t* function_getglb(function_t *func, const char *glb, char *name);
extern temp_t* function_getstr(function_t *func, char *str, char *name);
extern temp_t* function_findtemp(function_t *func, const char *name);

extern void function_printtemp(function_t *func, const char *name);
extern void function_labeltemp(function_t *func, const char *name);

#endif
