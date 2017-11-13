#ifndef STRING_H
#define STRING_H

typedef struct string_t {
    struct string_t *next;
    char *name;
    char *str;
} string_t;

extern string_t* string_init(char *name, char *str, string_t *next);
extern void string_destroy(string_t *string);

#endif
