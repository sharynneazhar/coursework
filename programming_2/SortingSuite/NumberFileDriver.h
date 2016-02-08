/**
*	@file : NumberFileDriver.h
*	@author : Sharynne Azhar
*	@date : 2015.10.26
*	@brief: header file for number file driver
*/

#ifndef NUMBERFILEDRIVER_H
#define NUMBERFILEDRIVER_H

#include <iostream>
#include "NumberFileGenerator.h"

class NumberFileDriver{

public:
	/*
	* @pre valid arguments
	* @post runs number file generator
	* @return none
    */
	static void run(int argc, char** argv);

	/*
	* @pre none
	* @post prints helpful menu
	* @return none
	*/
	static void printHelpMenu();

private:
	/*
	* @pre valid option
	* @post prints a menu to help user use NumberFileGenerator
	* @return true if it is valid option, false otherwise
	*/
	static bool isValidOption(std::string option);
};

#endif
