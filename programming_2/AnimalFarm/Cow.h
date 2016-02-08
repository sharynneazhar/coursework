/**
*	@file : Cow.h
*	@author : Sharynne Azhar
*	@date : 2015.10.05
*	@brief: Header file for the cow class
*/

#ifndef COW_H
#define COW_H

#include "FarmAnimal.h"

class Cow : public FarmAnimal
{
protected:
    double m_milkProduced;

public:
    /*
    * @pre none
    * @post initalizes all instances of protected member variables
    * @return none
    */
    Cow();

    /*
    * @pre none
    * @post none
    * @return m_milkProduced of type double
    */
    double getMilkProduced() const;

    /*
    * @pre gallons is greater than 0
    * @post assigns gallons of type double to m_milkProduced
    * @return none
    */
    void setMilkProduced(double gallons);
};

#endif
