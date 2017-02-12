lab3: main.o
	g++ -g -std=c++11 main.o -o lab3

main.o: main.cpp HashObject.h HashTable.h HashTable.hpp
	g++ -g -std=c++11 -c main.cpp

clean:
	rm *.o lab3
	echo clean done

test:
	./lab3 < input.txt
