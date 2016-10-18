/**
*	@file : Pokemon.cpp
*	@author : Sharynne Azhar
*	@date : 01-25-2016
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

void Pokemon::reduceHP(int amount)
{
	m_hp -= amount;
}

int Pokemon::getHP() const
{
	return(m_hp);
}

void Pokemon::setHP(int h)
{
	m_hp = h;
}

int Pokemon::getAttackLevel() const
{
	return(m_attackLevel);
}

void Pokemon::setAttackLevel(int a)
{
	m_attackLevel = a;
}

int Pokemon::getDefenseLevel() const
{
	return(m_defenseLevel);
}

void Pokemon::setDefenseLevel(int d)
{
	m_defenseLevel = d;
}

std::string Pokemon::getName() const
{
	return(m_name);
}

void Pokemon::setName(std::string n)
{
	m_name = n;
}
