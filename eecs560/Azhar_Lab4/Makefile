lab4: main.o
	g++ -g -std=c++11 main.o -o lab4

main.o: main.cpp Timer.h HashObject.h HashTable.h HashTable.hpp
	g++ -g -std=c++11 -c main.cpp

clean:
	rm *.o lab4
	echo clean done

test:
	./lab4 < input.txt
