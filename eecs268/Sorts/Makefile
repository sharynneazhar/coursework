Lab06: main.o Test.o
	g++ -g -std=c++11 main.o Test.o -o Lab06

main.o: main.cpp Test.h Sorts.h Sorts.hpp
	g++ -g -std=c++11 -c main.cpp

Test.o: Test.h Test.cpp
	g++ -g -std=c++11 -c Test.cpp

clean:
	rm *.o Lab06
	echo clean done
