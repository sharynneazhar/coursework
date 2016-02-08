Lab07: main.o NumberFileDriver.o NumberFileGenerator.o SortDriver.o Test.o
	g++ -g -std=c++11 -Wall main.o NumberFileDriver.o NumberFileGenerator.o SortDriver.o Test.o -o Lab07

main.o: main.cpp Sorts.h Sorts.hpp NumberFileDriver.h NumberFileGenerator.h SortDriver.h Test.h
	g++ -g -std=c++11 -Wall -c main.cpp

NumberFileDriver.o: NumberFileDriver.h NumberFileGenerator.h NumberFileGenerator.cpp
	g++ -g -std=c++11 -Wall -c NumberFileDriver.cpp

NumberFileGenerator.o: NumberFileGenerator.h NumberFileGenerator.cpp
	g++ -g -std=c++11 -Wall -c NumberFileGenerator.cpp

SortDriver.o: NumberFileGenerator.h SortDriver.h Sorts.h Sorts.hpp SortDriver.cpp
	g++ -g -std=c++11 -Wall -c SortDriver.cpp

Test.o: Test.h Test.cpp
	g++ -g -std=c++11 -Wall -c Test.cpp


clean:
	rm *.o *.dat *.out Lab07
	echo clean done
