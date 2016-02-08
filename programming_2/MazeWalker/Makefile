Lab09: main.o MazeReader.o MazeWalker.o MazeCreationException.o Position.o Test.o
	g++ -g -std=c++11 main.o MazeReader.o MazeWalker.o MazeCreationException.o Position.o Test.o -o Lab09

main.o: main.cpp MazeReader.h MazeWalker.h MazeCreationException.h Position.h Test.h
	g++ -g -std=c++11 -c main.cpp

MazeReader.o: MazeReader.h MazeReader.cpp
	g++ -g -std=c++11 -c MazeReader.cpp

MazeCreationException.o: MazeCreationException.h MazeCreationException.cpp
	g++ -g -std=c++11 -c MazeCreationException.cpp

MazeWalker.o: MazeWalker.h MazeWalker.cpp
	g++ -g -std=c++11 -c MazeWalker.cpp

Position.o: Position.h Position.cpp
	g++ -g -std=c++11 -c Position.cpp

Test.o: Test.h Test.cpp
	g++ -g -std=c++11 -c Test.cpp

clean:
	rm *.o Lab09
	echo clean done
