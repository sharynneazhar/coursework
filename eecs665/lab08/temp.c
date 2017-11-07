#include <stdio.h>
#include <stdlib.h>
#include <temp.h>

static temp_t* temp_init(char *name, temp_t *next) {
    temp_t *tmp = (temp_t*)malloc( sizeof(temp_t) );
    if (tmp == NULL) {
        perror("could not allocate memory for operand");
        exit(1);
    }

    tmp->name = name;
    tmp->next = next;
    return tmp;
}

temp_t* temp_param(char *name, arg_t *arg, temp_t *next) {
    temp_t *tmp = temp_init(name, next);
    tmp->type = OPERAND_PARAM;
    tmp->arg = arg;
    return tmp;
}

temp_t* temp_local(char *name, local_t *local, temp_t *next) {
    temp_t *tmp = temp_init(name, next);
    tmp->type = OPERAND_LOCAL;
    tmp->local = local;
    return tmp;
}

temp_t* temp_global(char *name, const char *glb, temp_t *next) {
    temp_t *tmp = temp_init(name, next);
    tmp->type = OPERAND_GLOBAL;
    tmp->glb = glb;
    return tmp;
}

temp_t* temp_int(char *name, long long val, temp_t *next) {
    temp_t *tmp = temp_init(name, next);
    tmp->type = OPERAND_INT;
    tmp->val = val;
    return tmp;
}

void temp_destroy(temp_t *tmp) {
    free(tmp);
}

void temp_print(temp_t *tmp) {
    switch (tmp->type) {
    default:
        fprintf(stderr, "INTERNAL COMPILER ERROR: " );
        fprintf(stderr, "unknown operand type %d\n", tmp->type );
        exit(1);

    case OPERAND_INT:    printf( "$%lld", tmp->val ); break;
    case OPERAND_GLOBAL: printf( "$%s", tmp->glb ); break;
    case OPERAND_PARAM:  arg_print(tmp->arg); break;
    case OPERAND_LOCAL:  local_print(tmp->local); break;
    }
}

void temp_label(temp_t *tmp) {
    switch (tmp->type) {
    default:
        fprintf(stderr, "INTERNAL COMPILER ERROR: " );
        fprintf(stderr, "unknown operand type %d\n", tmp->type );
        exit(1);

    case OPERAND_INT:    printf( "$%lld", tmp->val ); break;
    case OPERAND_GLOBAL: printf( "%s", tmp->glb ); break;
    case OPERAND_PARAM:  arg_print(tmp->arg); break;
    case OPERAND_LOCAL:  local_print(tmp->local); break;
    }
}
