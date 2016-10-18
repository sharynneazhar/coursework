Lab04: main.o Test.o PreconditionViolationException.o
	g++ -g -std=c++11 main.o PreconditionViolationException.o Test.o -o Lab04

main.o: main.cpp Test.h Node.h Node.hpp StackInterface.h Stack.h Stack.hpp PreconditionViolationException.h
	g++ -g -std=c++11 -c main.cpp

PreconditionViolationException.o: PreconditionViolationException.h PreconditionViolationException.cpp
	g++ -g -std=c++11 -c PreconditionViolationException.cpp

Test.o: Test.h Test.cpp
	g++ -g -std=c++11 -c Test.cpp

clean:
	rm *.o Lab04
	echo clean done
