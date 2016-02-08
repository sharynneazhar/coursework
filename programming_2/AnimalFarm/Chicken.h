/**
*	@file : Chicken.h
*	@author : Sharynne Azhar
*	@date : 2015.10.05
*	@brief: Header file for the Chicken class
*/

#ifndef CHICKEN_H
#define CHICKEN_H

#include "FarmAnimal.h"

class Chicken : public FarmAnimal
{
protected:
    /*
    * @pre none
    * @post none
    * @return m_eggs of type int
    */
    int getEggs() const;

    /*
    * @pre none
    * @post m_eggs is set to eggs
    * @return none
    */
    void setEggs(int eggs);

    int m_eggs;

public:
    /*
    * @pre none
    * @post initalizes all instances of member variables
    * @return none
    */
    Chicken();
};

#endif
