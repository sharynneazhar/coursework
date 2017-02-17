lab4: main.o
	g++ -Wall -g -std=c++11 main.o -o lab4

main.o: main.cpp Timer.h OpenHash.h ClosedHash.h
	g++ -Wall -g -std=c++11 -c main.cpp

clean:
	rm *.o lab4
	echo clean done

test:
	./lab4
