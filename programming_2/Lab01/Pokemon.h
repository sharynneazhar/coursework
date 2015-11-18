/**
*	@file : Pokemon.h
*	@author : Sharynne Azhar
*	@date : 2015.08.28
*	Purpose: Header file of Pokemon class. Used to store information on the user's pokemon.
*/

#ifndef POKEMON_H
#define POKEMON_H

#include <string>

using namespace std;

class Pokemon
{
private:
	int m_hp;
	int m_attackLevel;
	int m_defenseLevel;
	string m_name;

public: 
	/**
	* @pre None 
	* @post Initializes all numeric data to zero and strings to ""
	**/
	Pokemon();


	int getHP() const;
	int getAttackLevel() const;
	int getDefenseLevel() const;
	string getName() const;

	void setHP(int h);
	void setAttackLevel(int a);
	void setDefenseLevel(int d);
	void setName(string n);
	
	/**
	* @pre amount is determine by attack() in Colosseum class 
	* @post subtracts amount from m_hp 
	**/
	void reduceHP(int amount); 
};

#endif
