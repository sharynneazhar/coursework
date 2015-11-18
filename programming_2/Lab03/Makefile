Lab03: main.o Test.o
	g++ -g -std=c++11 main.o Test.o -o Lab03

main.o: main.cpp Test.h Node.h Node.hpp DoubleLinkedList.h DoubleLinkedList.hpp
	g++ -g -std=c++11 -c main.cpp

Test.o: Test.h Test.cpp
	g++ -g -std=c++11 -c Test.cpp

clean:
	rm *.o Lab03
	echo clean done
