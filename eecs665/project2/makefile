#CFLAGS= -g -DLEFTTORIGHT
CFLAGS= -O -DLEFTTORIGHT
CC=gcc -g

csem:	sym.o scan.o sem.o semutil.o cgram.o
	gcc -g -o csem sym.o scan.o sem.o semutil.o cgram.o

sym.o:	sym.c cc.h sym.h

scan.o:	scan.c cc.h scan.h y.tab.h

sem.o:	sem.c cc.h sem.h semutil.h sym.h

semutil.o: semutil.c cc.h sem.h sym.h

y.tab.h cgram.c: cgram.y
	yacc -vd cgram.y
	mv y.tab.c cgram.c

test:	
	./csem < test.c > test.out
	./sem_base.exe < test.c > expected.out
	diff test.out expected.out

clean:
	rm csem sym.o scan.o sem.o semutil.o cgram.o expected.out
