prog: main.o
	g++ -g -std=c++11 main.o -o prog

main.o: main.cpp Node.h Node.hpp
	g++ -g -std=c++11 -c main.cpp

clean:
	rm *.o prog
	echo clean done
