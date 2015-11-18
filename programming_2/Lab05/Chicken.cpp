/**
*	@file : Chicken.cpp
*	@author : Sharynne Azhar
*	@date : 2015.10.05
*	@brief: Implementation file for the Chicken class
*/

#include "Chicken.h"

int Chicken::getEggs() const
{
    return m_eggs;
}

void Chicken::setEggs(int eggs)
{
    m_eggs = eggs;
}

Chicken::Chicken()
{
    setName("Chicken");
    setSound("Cluck");
}
