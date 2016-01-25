/*
*	@file : Pokemon.h
*	@author : Sharynne Azhar
*	@date : 01-25-2016
    Purpose: Header file of Pokemon class. Used to store information about
             user's Pokemon
*/

#ifndef POKEMON_H
#define POKEMON_H

#include <string>

class Pokemon
{

private:
    int m_hp; // health points of the pokemon
    int m_attackLevel; // attack level of the pokemon
    int m_defenseLevel; // defense level of the pokemon
    std::string m_name; // name of the pokemon

public:
    /**
	* @pre None
	* @post Initializes all numeric data to zero and strings to empty
    * @return None
	**/
	Pokemon();

	/**
	* @pre amount is determine by attack() in Colosseum class
	* @post subtracts amount from m_hp
    * @return none
	**/
	void reduceHP(int amount);

    int getHP() const;
    void setHP(int h);

	int getAttackLevel() const;
    void setAttackLevel(int a);

	int getDefenseLevel() const;
    void setDefenseLevel(int d);

	std::string getName() const;
	void setName(std::string n);
};

#endif
