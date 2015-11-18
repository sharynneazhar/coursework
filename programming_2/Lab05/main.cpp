/**
*	@file : main.cpp
*	@author : Sharynne Azhar
*	@date : 2015.10.05
*	@brief: Implementation of driver program
*/

#include "Node.h"
#include "StackInterface.h"
#include "Stack.h"
#include "FarmAnimal.h"
#include "Cow.h"
#include "Chicken.h"
#include "CyberChicken.h"
#include "AnimalPen.h"

#include <iostream>
#include <string>
#include <stdexcept>

void goodbyeMessage(const FarmAnimal& animal);

int main()
{
    AnimalPen pen;
    bool notDone = true; // flag

    // add animals as long as user wants
    while (notDone)
    {
        std::cout<<"\nSelect and animal to add to the pen:\n"
			<<"1.) Cow (produces milk)\n"
			<<"2.) Chicken (cannot lay eggs)\n"
			<<"3.) Cyber Chicken (seems dangerous, but lays eggs)\n"
			<<"---------------------------------------------------\n"
			<<"choice: ";

        int menuChoice;
        std::cin >> menuChoice;

        switch (menuChoice)
        {
            case 1:
                {
                    Cow* addCow = new Cow();
                    double gallonsProduced;

                    std::cout << "How many gallons of milk did this cow produce? ";
                    std::cin >> gallonsProduced;

                    // set gallons produced by cow
                    addCow->setMilkProduced(gallonsProduced);

                    // add the cow to the animal pen
                    pen.push(static_cast<Cow*>(addCow));
                    break;
                }
            case 2:
                {
                    Chicken* addChicken = new Chicken();
                    std::cout << "Add an eggless chicken to the pen? OK. You're the boss.\n";

                    // add the chicken into the animal pen
                    pen.push(static_cast<FarmAnimal*>(addChicken));
                    break;
                }
            case 3:
                {
                    CyberChicken* addCyberChicken = new CyberChicken();
                    int numOfEggs;

                    std::cout << "How many eggs did this cyber chicken produce? ";
                    std::cin >> numOfEggs;

                    // set the number of eggs produced by cyber chicken
                    addCyberChicken->setCyberEggs(numOfEggs);

                    // add the cyber chicken to the pen
                    pen.push(static_cast<FarmAnimal*>(addCyberChicken));
                    break;
                }
        } // end switch

        char end;
        std::cout << "Done adding animals? (y/n) ";
        std::cin >> end;

        if (end == 'y' || end == 'Y')
        {
            notDone = false;
        }

        std::cout << "\n";
    } // end while

    // release animals!
    std::cout << "\nReleasing all animals!\n";
    std::cout << "---------------------------\n";

    double totalMilkProduced = 0.0;
    int totalEggsLaid = 0;

    while (!pen.isPenEmpty())
    {
        FarmAnimal* farmAnimal = pen.peekAtNextAnimal();
        if (farmAnimal->getName() == "Cow")
        {
            totalMilkProduced += static_cast<Cow*>(farmAnimal)->getMilkProduced();
            std::cout << "This cow produced ";
            std::cout << static_cast<Cow*>(farmAnimal)->getMilkProduced();
            std::cout << " gallons of milk.\n";
            goodbyeMessage(*farmAnimal);
            pen.pop();
        }
        else if (farmAnimal->getName() == "Chicken")
        {
            std::cout << "Chicken unable to lay eggs. Perhaps cybornetic implants will help?\n";
            goodbyeMessage(*farmAnimal);
            pen.pop();
        }
        else
        {
            totalEggsLaid += static_cast<CyberChicken*>(farmAnimal)->getCyberEggs();
            std::cout << "This Cyber Chicken laid ";
            std::cout << static_cast<CyberChicken*>(farmAnimal)->getCyberEggs();
            std::cout << " cyber eggs. Humanity is in trouble.\n";
            goodbyeMessage(*farmAnimal);
            pen.pop();
        }

        delete farmAnimal;
        farmAnimal = nullptr;
    } // end while

    std::cout<<"\n\nYour farm produced "<< totalMilkProduced << " gallons of milk and " << totalEggsLaid << " eggs.\n\n";

    return 0;
} // end main

void goodbyeMessage(const FarmAnimal& animal)
{
    std::cout << "Upon release, the " << animal.getName();
    std::cout << " said " << animal.getSound() << std::endl;
}
