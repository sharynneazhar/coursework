#ifndef SYM_H
#define SYM_H

#include <stdint.h>

void dump(int, FILE *);
void new_block();
void exit_block();
void enterblock();
struct id_entry *install(char *, int);
void leaveblock();
struct id_entry *lookup(char *, int);
void sdump(FILE *);
char *slookup(char []);
intptr_t hash(char *);
char *alloc(unsigned);
void save_rec(struct sem_rec *);

#endif
