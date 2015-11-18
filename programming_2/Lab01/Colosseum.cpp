/**
*	@file : Colosseum.cpp
*	@author : Sharynne Azhar
*	@date : 2015.08.28
*	Purpose: Implementation file of Colosseum class.
*/

#include "Colosseum.h"

#include <string>
#include <iostream>

using namespace std;

Colosseum::Colosseum()
{
	d20 = Dice(20);
	d6 = Dice(6);
}

void Colosseum::userBuild(Pokemon& p)
{
	string name = "";
	int hp = 0;
	int attack = 0;
	int defense = 0;

	cout << "===========================================" << endl;
	cout << "Please name your Pokemon: ";
	cin >> name;
	p.setName(name);

	cout << "How many hit points will it have? (1-50): ";
	cin >> hp;
	
	// error if HP input is out of range
	while (hp < 1 || hp > 50)
	{
		cout << "Invalid input." << endl;
		cout << "Please enter a number between 1 and 50:" << endl;
		cin >> hp;
	}
	p.setHP(hp);

	cout << endl;
	cout << "** Split 50 points between attack level and defense level **" << endl;
	cout << "Enter your attack level (1-49): ";
	cin >> attack;

	// error if attack input is out of range
	while (attack < 1 || attack > 50)
	{
		cout << "Invalid input." << endl;
		cout << "Enter a number between 1 and 50: ";
		cin >> attack;
	}
	p.setAttackLevel(attack);
	
	cout << "Enter your defense level (0-" << (50 - attack) <<  ") : ";
	cin >> defense;
	
	// error if defense input is out of range
	while (defense < 0 || defense > (50 - attack)) 
	{
		cout << "Invalid input." << endl;
		cout << "Enter a number between 0 and " << (50 - attack) << ": ";
		cin >> defense;
	}
	p.setDefenseLevel(defense);

	cout << "===========================================" << endl << endl;
}

bool Colosseum::attack(const Pokemon& attacker, Pokemon& defender)
{
	int damage = 0;
	int attackBonus = d20.roll(); // roll for attack bonus
	int defenseBonus = d20.roll(); // roll for defense bonus

	int attack = attacker.getAttackLevel();
	int defense = defender.getDefenseLevel();

	cout << attacker.getName() << " is attacking " << defender.getName() << endl;

	cout << attacker.getName() << " rolls an attack bonus of " << attackBonus << endl;
	cout << defender.getName() << " rolls an defense bonus of " << defenseBonus << endl;

	if ((attack + attackBonus) > (defense + defenseBonus))
	{
		// roll for damage
		int roll1 = d6.roll();
		int roll2 = d6.roll();
		int roll3 = d6.roll();

		damage = roll1 + roll2 + roll3;
		cout << "Attack hits dealing 3-D6 damage!" << endl;
		cout << "The rolls are " << roll1 << ", " << roll2 << ", and " << roll3 << "." << endl;
		cout << "Damage total: " << damage << "!" << endl;

		defender.reduceHP(damage);
	}
	else
	{
		cout << attacker.getName() << " missed the attack!" << endl;
	}

	if (defender.getHP() <= 0) 
	{
		cout << endl;
		cout << defender.getName() << " has been defeated!" << endl << endl;
		return true; // end game
	}
	else
	{
		cout << defender.getName() << " has " << defender.getHP() << " HP left." << endl;
		return false;
	}
}

void Colosseum::play(Pokemon& p1, Pokemon& p2)
{
	bool gameover = false; // gameover when true: player dead from attack
	int first;

	Dice d2 = Dice(2);
	d2.roll(); // rolls to see which player goes first

	if (d2.roll() == 1)
	{
		cout << "Player 1 will go first" << endl << endl;
		first = 1;
	}
	else
	{
		cout << "Player 2 will go first" << endl << endl;
		first = 2;
	}

	// loop until player dead
	for (int i = 0; i < 10 && !gameover; i++)
	{
		cout << endl;
		cout << "Round " << i + 1 << ": " << endl;
		
		if (first == 1)
		{
			gameover = attack(p1, p2);
			if (!gameover)
				gameover = attack(p2, p1);
		}
		else
		{
			gameover = attack(p2, p1);
			if (!gameover)
				gameover = attack(p1, p2);
		}
	}
}
