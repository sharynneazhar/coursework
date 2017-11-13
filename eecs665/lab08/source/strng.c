#include <stdio.h>
#include <stdlib.h>
#include <strng.h>

string_t* string_init(char *name, char *str, string_t *next) {
    string_t *string = (string_t*)malloc(sizeof(string_t));
    if (string == NULL) {
        perror("cannot allocate memory for formal stringument");
        exit(1);
    }

    string->name = name;
    string->str = str;
    string->next = next;
    return string;
}

void string_destroy(string_t *string) {
    free(string->name);
    free(string->str);
    free(string);
}
