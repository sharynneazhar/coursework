/**
*	@file : Colosseum.cpp
*	@author : Sharynne Azhar
*	@date : 01-25-2016
*	Purpose: Implementation of Colosseum class.
*/

#include "Colosseum.h"

Colosseum::Colosseum()
{
	d20 = Dice(20);
	d6 = Dice(6);
}

void Colosseum::userBuild(Pokemon& p)
{
	std::string name = "";
	int hp = 0;
	int attack = 0;
	int defense = 0;

	std::cout << "===========================================" << std::endl;
	std::cout << "Please name your Pokemon: ";
	std::cin >> name;
	p.setName(name);

	std::cout << "How many health points will it have? (1-50): ";
	std::cin >> hp;

	// error if HP input is out of range
	while (hp < 1 || hp > 50)
	{
		std::cout << "Invalid input." << std::endl;
		std::cout << "Please enter a number between 1 and 50:" << std::endl;
		std::cin >> hp;
	}
	p.setHP(hp);

	std::cout << std::endl;
	std::cout << "** Split 50 points between attack level and defense level **" << std::endl;
	std::cout << "Enter your attack level (1-49): ";
	std::cin >> attack;

	// error if attack input is out of range (1-49)
	while (attack < 1 || attack > 50)
	{
		std::cout << "Invalid input." << std::endl;
		std::cout << "Enter a number between 1 and 50: ";
		std::cin >> attack;
	}

	p.setAttackLevel(attack);

	std::cout << "Enter your defense level (0-" << (50 - attack) <<  ") : ";
	std::cin >> defense;

	// error if defense input is out of range
	while (defense < 0 || defense > (50 - attack))
	{
		std::cout << "Invalid input." << std::endl;
		std::cout << "Enter a number between 0 and " << (50 - attack) << ": ";
		std::cin >> defense;
	}

	p.setDefenseLevel(defense);

	std::cout << "===========================================" << std::endl << std::endl;
}

bool Colosseum::attack(const Pokemon& attacker, Pokemon& defender)
{
	int damage = 0;
	int attackBonus = d20.roll(); // roll for attack bonus
	int defenseBonus = d20.roll(); // roll for defense bonus

	int attack = attacker.getAttackLevel();
	int defense = defender.getDefenseLevel();

	std::cout << attacker.getName() << " is attacking " << defender.getName() << std::endl;

	std::cout << attacker.getName() << " rolls an attack bonus of "
              << attackBonus << std::endl;
	std::cout << defender.getName() << " rolls an defense bonus of "
              << defenseBonus << std::endl;

	if ((attack + attackBonus) > (defense + defenseBonus))
	{
		// roll for damage
		int roll1 = d6.roll();
		int roll2 = d6.roll();
		int roll3 = d6.roll();

		damage = roll1 + roll2 + roll3;
		std::cout << "Attack hits dealing 3-D6 damage!" << std::endl;
		std::cout << "The rolls are " << roll1 << ", " << roll2
                  << ", and " << roll3 << "." << std::endl;
		std::cout << "Damage total: " << damage << "!" << std::endl;

		defender.reduceHP(damage);
	}
	else
	{
		std::cout << attacker.getName() << " missed the attack!" << std::endl;
	}

	if (defender.getHP() <= 0)
	{
		std::cout << std::endl;
		std::cout << defender.getName() << " has been defeated!" << std::endl << std::endl;
		return true; // end game
	}
	else
	{
		std::cout << defender.getName() << " has " << defender.getHP()
                  << " HP left." << std::endl;
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
		std::cout << "Player 1 will go first" << std::endl << std::endl;
		first = 1;
	}
	else
	{
		std::cout << "Player 2 will go first" << std::endl << std::endl;
		first = 2;
	}

	// loop until player dead, 10 round fight
	for (int i = 0; i < 10 && !gameover; i++)
	{
		std::cout << std::endl;
		std::cout << "Round " << i + 1 << ": " << std::endl;

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
