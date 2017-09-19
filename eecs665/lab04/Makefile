LEX=lex
YACC=yacc
CC=gcc

LFLAGS=
CFLAGS=-Wall -Wno-unused-label -Wno-unused -g

lexer: lexer.lex.o
	$(CC) -o $@ $+

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

%.lex.c: %.l
	$(LEX) $(LFLAGS) -o$@ $<

test_diff: lexer test.c
	./lexer < test.c > test.expected_out
	diff test.out test.expected_out

clean:
	$(RM) *.o
	$(RM) *.lex.c
	$(RM) lexer

.PHONY: clean all test_diff demo_april demo_kurt

# Demo rules
demo_april: lexer
	./lexer < /home/t982w485/lab04_demo.c > demo.out
	diff demo.out /home/t982w485/lab04_demo.out

demo_kurt: lexer
	./lexer < /home/k287s129/lab04_demo.c > demo.out
	diff demo.out /home/k287s129/lab04_demo.out
