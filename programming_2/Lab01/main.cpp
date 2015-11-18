#include "Pokemon.h"
#include "Colosseum.h"

#include <iostream>

using namespace std;

int main()
{
	char play_again;

	do
	{
		Pokemon player1;
		Pokemon player2;
		Colosseum colosseum;
		
		cout << "*** BUILD YOUR POKEMON ***" << endl << endl;	
	 
		cout << "Player 1, build your Pokemon!" << endl;
		colosseum.userBuild(player1);

		cout << "Player 2, build your Pokemon!" << endl;
		colosseum.userBuild(player2);
		
		cout << "*** BATTLE COMMENCE ***" << endl << endl;

		colosseum.play(player1, player2);
		
		cout << "Would you like to play again? (Y/N): ";
		cin >> play_again;
		cout << endl;

	}while (play_again != 'N' && play_again != 'n'); // loop until user wants to quit

	return (0);
}