prog: main.o
	g++ -g -std=c++11 main.o -o prog

main.o: main.cpp Barrier.h
	g++ -g -std=c++11 -c main.cpp

clean:
	rm *.o prog
	echo clean done

run:
	./prog input.txt
