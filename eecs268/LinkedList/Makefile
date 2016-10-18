lab03: main.o Node.o LinkedList.o Test.o Test_LinkedList.o
	g++ -std=c++11 -g -Wall main.o Node.o LinkedList.o Test.o Test_LinkedList.o -o lab03

main.o: main.cpp Node.h LinkedList.h Test.h Test_LinkedList.cpp
	g++ -std=c++11 -g -Wall -c main.cpp

Node.o: Node.h Node.cpp
	g++ -std=c++11 -g -Wall -c Node.cpp

LinkedList.o: LinkedList.h LinkedList.cpp
	g++ -std=c++11 -g -Wall -c LinkedList.cpp

Test.o: Test.h Test.cpp
	g++ -std=c++11 -g -Wall -c Test.cpp

Test_LinkedList.o: Test_LinkedList.h Test_LinkedList.cpp
	g++ -std=c++11 -g -Wall -c Test_LinkedList.cpp

clean:
	rm *.o lab03
	echo clean done
