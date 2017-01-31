lab2: main.o
	g++ -g -std=c++11 main.o -o lab2

main.o: main.cpp Node.h Node.hpp DoubleLinkedList.h DoubleLinkedList.hpp
	g++ -g -std=c++11 -c main.cpp

clean:
	rm *.o lab2
	echo clean done
