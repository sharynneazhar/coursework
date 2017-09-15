LEX=lex
YACC=yacc
CC=gcc

LFLAGS=
CFLAGS=-Wall -Wno-unused-label -Wno-unused -g

all: html2latex

html2latex: html2latex.lex.o
	$(CC) -o $@ $+

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

%.lex.c: %.l
	$(LEX) $(LFLAGS) -o$@ $<

%.tex: %.html html2latex
	./html2latex < $< > $@

%.pdf: %.tex
	pdflatex $<

test: html2latex test.pdf

clean:
	$(RM) *.o
	$(RM) *.lex.c
	$(RM) *.tex *.log *.aux *.pdf
	$(RM) html2latex

.PHONY: clean all test

# Demo rules
demo_april.tex: html2latex
	./html2latex < /home/t982w485/lab03_demo.html > demo_april.tex

demo_kurt.tex: html2latex
	./html2latex < /home/k287s129/lab03_demo.html > demo_kurt.tex

demo_april: test.pdf demo_april.pdf
	xdg-open test.pdf
	xdg-open demo_april.pdf

demo_kurt: test.pdf demo_kurt.pdf
	xdg-open test.pdf
	xdg-open demo_kurt.pdf
