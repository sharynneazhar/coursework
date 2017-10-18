LEX=lex
YACC=yacc
CC=gcc

LFLAGS=
YFLAGS=
CFLAGS=-Wall -Wno-unused-label -Wno-unused -I. -DNDEBUG

all: fcg

fcg: fcg.yacc.o fcg.lex.o
	$(CC) -o $@ $+

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

%.lex.c: %.l
	$(LEX) $(LFLAGS) -o$@ $<

%.yacc.c: %.y
	$(YACC) $(YFLAGS) -d -o$@ $<

%.dot: %.c fcg
	./fcg < $< > $@

%.png: %.dot
	dot -Tpng -o $@ $<

test: test.png

clean:
	$(RM) *.o
	$(RM) *.lex.c
	$(RM) *.yacc.c
	$(RM) *.yacc.h
	$(RM) *.yacc.output
	$(RM) *.dot
	$(RM) *.png
	$(RM) fcg

.PHONY: clean all test


demo_april: fcg
	./fcg < /home/t982w485/lab05_demo.c > demo.dot
	dot -Tpng -o demo.png demo.dot
	xdg-open demo.png

demo_kurt: fcg
	./fcg < /home/k287s129/lab05_demo.c > demo.dot
	dot -Tpng -o demo.png demo.dot
	xdg-open demo.png
