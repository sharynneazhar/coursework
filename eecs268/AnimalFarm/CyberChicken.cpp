/**
*	@file : CyberChicken.cpp
*	@author : Sharynne Azhar
*	@date : 2015.10.05
*	@brief: Implementation file for the CyberChicken class
*/

#include "CyberChicken.h"

CyberChicken::CyberChicken()
{
    setName("Cyber Chicken");
    setSound("Resistance is futile");
}

int CyberChicken::getCyberEggs() const
{
    return m_eggs;
}

void CyberChicken::setCyberEggs(int eggs)
{
    setEggs(eggs);
}
