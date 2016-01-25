lab01: main.o Dice.o Pokemon.o Colosseum.o
	g++ -std=c++11 -g -Wall main.o Dice.o Pokemon.o Colosseum.o -o lab01

main.o: main.cpp Dice.h
	g++ -std=c++11 -g -Wall -c main.cpp

Dice.o: Dice.h Dice.cpp
	g++ -std=c++11 -g -Wall -c Dice.cpp

Pokemon.o: Pokemon.h Pokemon.cpp
	g++ -std=c++11 -g -Wall -c Pokemon.cpp

Colosseum.o: Colosseum.h Colosseum.cpp
	g++ -std=c++11 -g -Wall -c Colosseum.cpp

clean:
	rm *.o lab01
	echo clean done
