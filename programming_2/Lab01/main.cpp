/*
*	@file : main.cpp
*	@author : Sharynne Azhar
*	@date : 01-25-2016
    Purpose: Main driver - two player console based video game where two
             pokemon are created and fight each other
*/

#include <iostream>

#include "Pokemon.h"
#include "Colosseum.h"

int main()
{
	char play_again;

	do
	{
        Colosseum colosseum;
		Pokemon player1;
		Pokemon player2;

		std::cout << "*** BUILD YOUR POKEMON ***" << std::endl << std::endl;

		std::cout << "Player 1, build your Pokemon!" << std::endl;
		colosseum.userBuild(player1);

		std::cout << "Player 2, build your Pokemon!" << std::endl;
		colosseum.userBuild(player2);

		std::cout << "*** BATTLE COMMENCE ***" << std::endl << std::endl;

		colosseum.play(player1, player2);

		std::cout << "Would you like to play again? (Y/N): ";
		std::cin >> play_again;
		std::cout << std::endl;

	} while(play_again != 'N' && play_again != 'n'); // loop until user wants to quit

	return 0;
} // end main
