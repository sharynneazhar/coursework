/**
*	@file : Pokemon.cpp
*	@author : Sharynne Azhar
*	@date : 2015.08.28
*	Purpose: Implementation of Pokemon class.
*/

#include "Pokemon.h"

Pokemon::Pokemon()
{
	m_hp = 0;
	m_attackLevel = 0;
	m_defenseLevel = 0;
	m_name = "";
}

int Pokemon::getHP() const
{
	return(m_hp);
}

int Pokemon::getAttackLevel() const
{
	return(m_attackLevel);
}

int Pokemon::getDefenseLevel() const
{
	return(m_defenseLevel);
}

string Pokemon::getName() const
{
	return(m_name);
}

void Pokemon::setHP(int h)
{
	m_hp = h;
}

void Pokemon::setAttackLevel(int a)
{
	m_attackLevel = a;
}

void Pokemon::setDefenseLevel(int d)
{
	m_defenseLevel = d;
}

void Pokemon::setName(string n)
{
	m_name = n;
}

void Pokemon::reduceHP(int amount)
{
	m_hp -= amount;
}
