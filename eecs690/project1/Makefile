prog: main.o
	g++ -std=c++11 -g -Wall main.o -o prog -lpthread

main.o: main.cpp Barrier.h Train.h
	g++ -std=c++11 -g -Wall -c main.cpp

clean:
	rm *.o prog
	echo clean done

run:
	./prog routes.txt
