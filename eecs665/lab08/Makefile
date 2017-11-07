LEX=flex
YACC=bison
CC=gcc

LFLAGS=
YFLAGS=
CFLAGS=-Wall -Wno-unused-label -std=gnu99 -I. -g

all: cgen

cgen: main.o function.o arg.o local.o temp.o strng.o cgen.yacc.o cgen.lex.o
	$(CC) -o $@ $^

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

%.lex.c: %.l
	$(LEX) $(LFLAGS) -o$@ $<

%.yacc.c: %.y
	$(YACC) $(YFLAGS) -d -o$@ $<

test: cgen test.c
	./cgen test.t test.t.s
	gcc -std=gnu99 -g -march=pentium -m32 -o $@ test.c test.t.s

runtest: cgen
	./cgen test.t

test.gcc.out: test.c
	gcc -std=gnu99 -march=pentium -m32 -S -o $@ $<

clean:
	$(RM) -r *.o *.dSYM
	$(RM) *.s *.S
	$(RM) *.lex.c
	$(RM) *.yacc.c
	$(RM) *.yacc.h
	$(RM) cgen testex test

.PHONY: clean all test
