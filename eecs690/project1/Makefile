prog: main.o
	g++ -Wall -g -std=c++11 main.o -o prog

main.o: main.cpp Barrier.h Train.h
	g++ -Wall -g -std=c++11 -c main.cpp

clean:
	rm *.o prog
	echo clean done

run:
	./prog routes.txt
