prog: main.o
	g++ -g -Wall -std=c++11 -lpthread main.o -o prog

main.o: main.cpp Barrier.h Train.h
	g++ -g -Wall -std=c++11 -lpthread -c main.cpp

clean:
	rm *.o prog
	echo clean done

run:
	./prog routes.txt
