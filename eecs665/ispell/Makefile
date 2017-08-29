all:
	gcc -g -w defmt.c hash.c lookup.c tgood.c correct.c dump.c good.c ispell.c makedent.c term.c tree.c xgets.c -o ispell

execute:
	./ispell -a -d americanmed+ < small.txt > ispell.out

diff:
	diff ispell.out ispell.std.out

clean:
	rm -rf *.o ispell ispell.out
