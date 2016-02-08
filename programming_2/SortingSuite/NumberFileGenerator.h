/**
*	@file : NumberFileGenerator.h
*	@author : Sharynne Azhar
*	@date : 2015.10.26
*	@brief: Header file for NumberFileGenerator class
*/

#ifndef NUMBERFILEGENERATOR_H
#define NUMBERFILEGENERATOR_H

#include <fstream>
#include <random>
#include <ctime>
#include <string>

class NumberFileGenerator
{
public:
	/*
	* @pre fileName is valid path/filename. Amount is greater than zero
	* @post file is created with the amount of number is ascending order.
	* The amount of numbers in the file is the first number in the file
	* @return none
	*/
	static void ascending(std::string fileName, int amount);

	/*
	* @pre ileName is valid path/filename. Amount is greater than zero
	* @post file is created with the amount of number is descending order.
	* The amount of numbers in the file is the first number in the file
	* @return none
	*/
	static void descending(std::string fileName, int amount);

	/*
	* @pre fileName is valid path/filename. Amount is greater than zero
	* @post file is created with the specified amount of numbers
	* The amount of numbers in the file is the first number in the file
	* @return none
	*/
	static void random(std::string fileName, int amount, int min, int max);

	/*
	* @pre fileName is valid path/filename. Amount is greater than zero
	* @post file created wiht single number amount of times
	* The amount of numbers in the file is the first number in the file
	* @return none
	*/
	static void singleValue(std::string fileName, int amount, int value);
};

#endif
