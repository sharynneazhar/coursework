/**
*	@file : Colosseum.h
*	@author : Sharynne Azhar
*	@date : 01-25-2016
*	Purpose: Header file for Colosseum Class. Used to simulate the combat play
             between two pokemon
*/

#ifndef COLOSSEUM_H
#define COLOSSEUM_H

#include <iostream>
#include <string>

#include "Dice.h"
#include "Pokemon.h"

class Colosseum
{
    
private:
	Dice d20;
	Dice d6;

public:
	/**
	* @pre Dice class
	* @post Initializes dice to appropriate sizes
    * @return none
	**/
	Colosseum();

	/**
	* @pre p is of class Pokemon
	* @post Creates a Pokemon instance
    * @return none
	**/
	void userBuild(Pokemon& p);

	/**
	* @pre Two instances of class Pokemon
	* @post Determines attack information
    * @return true if opponent died from attack, false otherwise
	**/
	bool attack(const Pokemon& attacker, Pokemon& defender);

	/**
	* @pre Two instances of class Pokemon and Dice with 2 sides
	* @post Handles attack rounds and determines when game is over
    * @return none
	**/
	void play(Pokemon& p1, Pokemon& p2);
};

#endif
