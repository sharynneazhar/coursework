Lab08: main.o Test.o
	g++ -g -std=c++11 main.o Test.o -o Lab08

main.o: main.cpp BinarySearchTree.h BinarySearchTree.hpp Test.h Node.h Node.hpp
	g++ -g -std=c++11 -c main.cpp

Test.o: Test.h Test.cpp
	g++ -g -std=c++11 -c Test.cpp

clean:
	rm *.o Lab08
	echo clean done
