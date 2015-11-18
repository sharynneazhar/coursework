/**
*	@file : SortDriver.h
*	@author : Sharynne Azhar
*	@date : 2015.10.26
*	@brief: Header file for sort driver
*/

#ifndef SORTDRIVER_H
#define SORTDRIVER_H

#include <iostream>
#include <string>
#include <fstream>

#include "Sorts.h"

class SortDriver
{
public:
    /*
	* @pre arguments is a valid 2D array and arguments are valid
	* @post file is created correctly with timing information
	* @return none
	*/
	static void run(int argc, char** argv);

	/*
	* @pre none
	* @post prints menu
	* @return none
	*/
	static void printHelpMenu();

private:
    /*
	* @pre input file was created by a NumberFileGenerator
	* @post none
	* @return true if the file exists, false otherwise
	*/
	static bool isFileAccessible(std::string fileName);

	/*
	* @pre none
	* @post none
	* @return true if the sort parameter matches a valid one
	*/
	static bool isSortValid(std::string sortParameter);

	/*
	* @pre none
	* @post none
	* @return true if file specified by inputFileName exists and sort parameter is valid
    */
	static bool areParametersValid(std::string sortName, std::string inputFileName);

	/*
	* @pre input file was created with the Number File Generator
	* @post first line of the file is read in, containing the count.
	* @return numbers in the file
	*/
	static int getFileCount(std::ifstream& inputFile);

	/*
	* @pre input file was created with the Number File
	* Generator, the size was read in, and that size if correct.
	* @post remainder of input file numbers are read in. File is NOT closed.
	* @return pointer to a new array containing the value from the input file
	*/
	static int* createArray(std::ifstream& inputFile, int size);

	/*
	* @pre original and copy are valid arrays of the correct size.
	* @post all values from original are copied into copy.
	* @return none
	*/
	static void copyArray(int original[], int copy[], int size);
};

#endif
