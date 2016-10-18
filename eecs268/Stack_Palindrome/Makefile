lab04: main.o PreconditionViolationException.o Test.o Test_Stack.o
	g++ -std=c++11 -g -Wall main.o PreconditionViolationException.o Test.o Test_Stack.o -o lab04

main.o: main.cpp Test.h Test_Stack.h Node.h Node.hpp Stack.h Stack.hpp PreconditionViolationException.h
	g++ -std=c++11 -g -Wall -c main.cpp

PreconditionViolationException.o: PreconditionViolationException.h PreconditionViolationException.cpp
	g++ -std=c++11 -g -Wall -c PreconditionViolationException.cpp

Test.o: Test.h Test.cpp
	g++ -std=c++11 -g -Wall -c Test.cpp

Test_Stack.o: Test_Stack.h Test_Stack.cpp
	g++ -std=c++11 -g -Wall -c Test_Stack.cpp

clean:
	rm *.o lab04
	echo clean done
