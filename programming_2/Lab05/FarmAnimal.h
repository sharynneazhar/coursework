/**
*	@file : FarmAnimal.h
*	@author : Sharynne Azhar
*	@date : 2015.10.05
*	@brief: Header file for the FarmAnimal class
*/

#ifndef FARMANIMAL_H
#define FARMANIMAL_H

#include <string>

class FarmAnimal
{
protected:
    std::string m_name;
    std::string m_sound;

public:
    /*
    * @pre none
    * @post initalizes all instances of protected member variables
    * @return none
    */
    FarmAnimal();

    /*
    * @pre none
    * @post none
    * @return m_name of the farm animal
    */
    std::string getName() const;

    /*
    * @pre none
    * @post assigns name to m_name
    * @return none
    */
    void setName(std::string name);

    /*
    * @pre none
    * @post none
    * @return m_sound of the farm animal
    */
    std::string getSound() const;

    /*
    * @pre none
    * @post assigns sound to m_sound
    * @return none
    */
    void setSound(std::string sound);
};

#endif
