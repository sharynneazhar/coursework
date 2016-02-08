/**
*	@file : Cow.h
*	@author : Sharynne Azhar
*	@date : 2015.10.05
*	@brief: Implementation file for the cow class
*/

#include "Cow.h"

Cow::Cow()
{
    setName("Cow");
    setSound("Moo");
    m_milkProduced = 0;
}

double Cow::getMilkProduced() const
{
    return m_milkProduced;
}

void Cow::setMilkProduced(double gallons)
{
    m_milkProduced = gallons;
}
