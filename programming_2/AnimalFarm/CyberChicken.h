/**
*	@file : CyberChicken.h
*	@author : Sharynne Azhar
*	@date : 2015.10.05
*	@brief: Header file for the CyberChicken class
*/

#ifndef CYBERCHICKEN_H
#define CYBERCHICKEN_H

#include "Chicken.h"

class CyberChicken : public Chicken
{
public:
    /*
    * @pre none
    * @post initalizes all instances of member variables
    * @return none
    */
    CyberChicken();
    
    /*
    * @pre none
    * @post none
    * @return m_eggs from Chicken class of type int
    */
    int getCyberEggs() const;

    /*
    * @pre none
    * @post sets m_eggs from Chicken class to eggs
    * @return none
    */
    void setCyberEggs(int eggs);
};

#endif
