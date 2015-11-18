/**
*	@file : Animal.h
*	@author : Sharynne Azhar
*	@date : 2015.10.05
*	@brief: Header file for the Animal class used to store pointers to animals on the heap
*/

#ifndef ANIMALPEN_H
#define ANIMALPEN_H

#include "Stack.h"
#include "FarmAnimal.h"

class AnimalPen : public Stack<FarmAnimal*>
{
public:
    /*
    * @pre none
    * @post none
    * @return none
    */
    AnimalPen();

    /*
    * @pre none
    * @post deletes all the animal in the stack
    * @return none
    */
    ~AnimalPen();

    /*
    * @pre assumes animal is type FarmAnimal
    * @post adds an animal instance into the stack
    * @return none
    */
    void addAnimal(FarmAnimal* animal);

    /*
    * @pre none
    * @post pointer instance assigned to the next animal
    * @return a pointer to the next animal
    */
    FarmAnimal* peekAtNextAnimal();

    /*
    * @pre the animal pen (stack) is not empty
    * @post deletes animal object and removes the animal pointer from the stack
    * @return none
    */
    void releaseAnimal();

    /*
    * @pre none
    * @post none
    * @return true if the pen is empty
    */
    bool isPenEmpty();
};

#endif
