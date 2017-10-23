CFLAGS= -g -DLEFTTORIGHT
#CFLAGS= -O -DLEFTTORIGHT
CC=gcc

csym:	sym.o scan.o sem_sym.o semutil.o cgram.o
	gcc -o csym sym.o scan.o sem_sym.o semutil.o cgram.o

sym.o:	sym.c cc.h sym.h

scan.o:	scan.c cc.h scan.h y.tab.h

sem_sym.o:	sem_sym.c cc.h sem.h semutil.h sym.h

semutil.o: semutil.c cc.h sem.h sym.h

y.tab.h cgram.c: cgram.y
	yacc -vd cgram.y >/dev/null 2>&1
	mv y.tab.c cgram.c

test:
	./csym < temp.c

clean:
	$(RM) csym sym.o scan.o sem_sym.o semutil.o cgram.o y.tab.h cgram.c y.output

demo_april: csym
	./csym < /home/t982w485/lab06_demo.c > demo.out
	diff demo.out /home/t982w485/lab06_demo.out

demo_kurt: csym
	./csym < /home/k287s129/lab06_demo.c > demo.out
	diff demo.out /home/k287s129/lab06_demo.out
