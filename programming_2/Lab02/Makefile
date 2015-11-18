Lab02: main.o Node.o LinkedList.o Test.o
	g++ -g -std=c++11 main.o Node.o LinkedList.o Test.o -o Lab02
	
main.o: main.cpp Node.h LinkedList.h Test.h
	g++ -g -std=c++11 -c main.cpp
	
Node.o: Node.h Node.cpp
	g++ -g -std=c++11 -c Node.cpp
	
LinkedList.o: LinkedList.h LinkedList.cpp
	g++ -g -std=c++11 -c LinkedList.cpp
	
Test.o: Test.h Test.cpp
	g++ -g -std=c++11 -c Test.cpp
	
clean: 
	rm *.o Lab02
	echo clean done
