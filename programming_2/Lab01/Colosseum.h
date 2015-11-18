/**
*	@file : Colosseum.h
*	@author : Sharynne Azhar
*	@date : 2015.08.28
*	Purpose: Header file of Colosseum class. Used to simulate pokemon battle.
*/

#ifndef COLOSSEUM_H
#define COLOSSEUM_H

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
	**/
	Colosseum();

	/**
	* @pre p is of class Pokemon 
	* @post Creates a Pokemon instance
	**/
	void userBuild(Pokemon& p);

	/**
	* @pre Two instances of class Pokemon  
	* @post Determines attack information
	**/
	bool attack(const Pokemon& attacker, Pokemon& defender);
	
	/**
	* @pre Two instances of class Pokemon and Dice with 2 sides
	* @post Handles attack rounds and determines when game is over
	**/
	void play(Pokemon& p1, Pokemon& p2);
};

#endif