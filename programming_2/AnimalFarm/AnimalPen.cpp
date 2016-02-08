/**
*	@file : Animal.cpp
*	@author : Sharynne Azhar
*	@date : 2015.10.05
*	@brief: Implementation file for the AnimalPen class
*/

#include "AnimalPen.h"

AnimalPen::AnimalPen(){} // stack does nothing

AnimalPen::~AnimalPen()
{
    while (!isEmpty())
    {
        releaseAnimal();
    }
}

void AnimalPen::addAnimal(FarmAnimal* animal)
{
    push(animal);
}

FarmAnimal* AnimalPen::peekAtNextAnimal()
{
    return (peek());
}

void AnimalPen::releaseAnimal()
{
    FarmAnimal* temp = peek();
    delete temp;
    temp = nullptr;
    pop();
}

bool AnimalPen::isPenEmpty()
{
    return (isEmpty());
}
