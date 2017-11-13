#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <function.h>

unsigned next_str = 0;
function_t *function;

function_t* function_init(char *name) {
    function_t *func = (function_t*)malloc( sizeof(function_t) );
    if (func == NULL) {
        perror("could not allocate memory for function definintion");
        exit(1);
    }

    func->name = name;
    func->args = NULL;
    func->locals = NULL;
    func->temps = NULL;
    func->strings = NULL;

    func->arg_offset = STACK_SIZE + 8;
    func->local_offset = STACK_SIZE;

    return func;
}

void function_destroy(function_t *func) {
    free(func->name);

    for (arg_t *atmp, *arg = func->args; arg != NULL; arg=atmp) {
        atmp = arg->next;
        arg_destroy(arg);
    }

    for (local_t *ltmp, *loc = func->locals; loc != NULL; loc=ltmp) {
        ltmp = loc->next;
        local_destroy(loc);
    }

    for (temp_t *ttmp, *tmp = func->temps; tmp != NULL; tmp=ttmp) {
        ttmp = tmp->next;
        temp_destroy(tmp);
    }
}

void function_dump(function_t *func) {
    printf("Function: %s\n", func->name);
    printf("--------------------------------------------------------------\n");

    for (arg_t *arg = func->args; arg != NULL; arg=arg->next) {
        printf("    ");
        arg_dump(arg);
    }

    for (local_t *loc = func->locals; loc != NULL; loc=loc->next) {
        printf("    ");
        local_dump(loc);
    }

    for (temp_t *tmp = func->temps; tmp != NULL; tmp=tmp->next) {
    }
}

arg_t* function_newarg(function_t *func, long long size) {
    func->args = arg_init(size, func->arg_offset, func->args);

    func->arg_offset += size;
    return func->args;
}

local_t* function_newlocal(function_t *func, long long size) {
    func->local_offset -= size;
    if (func->local_offset < 0) {
        fprintf(stderr, "INTERNAL COMPILER ERROR: ");
        fprintf(stderr, "function %s has too many locals\n", func->name);
        exit(1);
    }

    func->locals = local_init(size, func->local_offset, func->locals);
    return func->locals;
}

temp_t* function_getarg(function_t *func, unsigned num, char *name) {
    arg_t *arg = func->args;
    for (unsigned cur = 1; arg != NULL && cur < num; ++cur ) arg = arg->next;

    if (arg == NULL) {
        fprintf(stderr, "INTERNAL COMPILER ERROR: " );
        fprintf(stderr, "requested argument %d does not exist\n", num );
        exit(1);
    }

    func->temps = temp_param(name,arg,func->temps);
    return func->temps;
}

temp_t* function_getlocal(function_t *func, unsigned num, char *name) {
    local_t *loc = func->locals;
    for (unsigned cur = 1; loc != NULL && cur < num; ++cur ) loc = loc->next;

    if (loc == NULL) {
        fprintf(stderr, "INTERNAL COMPILER ERROR: " );
        fprintf(stderr, "requested argument %d does not exist\n", num );
        exit(1);
    }

    func->temps = temp_local(name,loc,func->temps);
    return func->temps;
}

temp_t* function_gettemp(function_t *func, long long size, char *name) {
    local_t *loc = function_newlocal(func, size);
    func->temps = temp_local(name,loc,func->temps);
    return func->temps;
}

temp_t* function_getint(function_t *func, long long val, char *name) {
    func->temps = temp_int(name,val,func->temps);
    return func->temps;
}

temp_t* function_getglb(function_t *func, const char *glb, char *name) {
    func->temps = temp_global(name,glb,func->temps);
    return func->temps;
}

temp_t* function_getstr(function_t *func, char *str, char *name) {
    char *nme;
    if (asprintf( &nme, "strval%u", next_str++) < 0) {
        perror("cannot allocate memory for string");
        exit(1);
    }

    func->strings = string_init(nme,str,func->strings);
    func->temps = temp_global(name,nme,func->temps);
    return func->temps;
}

temp_t* function_findtemp(function_t *func, const char *name) {
    for (temp_t *tmp = func->temps; tmp != NULL; tmp=tmp->next) {
        if (strcmp(tmp->name,name) == 0) return tmp;
    }

    fprintf(stderr, "INTERNAL COMPILER ERROR: " );
    fprintf(stderr, "could not find the temporary %s\n", name );
    exit(1);
}

void function_printtemp(function_t *func, const char *name) {
    temp_t *tmp = function_findtemp(func,name);
    temp_print(tmp);
}

void function_labeltemp(function_t *func, const char *name) {
    temp_t *tmp = function_findtemp(func,name);
    temp_label(tmp);
}
