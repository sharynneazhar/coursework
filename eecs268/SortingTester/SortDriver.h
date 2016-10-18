/**
*	@file : SortDriver.h
*	@author : Sharynne Azhar
*	@date : 04-11-2016
*	@brief: Header file for SortDriver class used to run the sorting algorithms
*/

#ifndef SORTDRIVER_H
#define SORTDRIVER_H

#include <iostream>
#include <fstream>
#include <string>
#include <stdlib.h>
#include <time.h>

#include "ArrayGenerator.h"
#include "sorts/bubbleSort.cpp"
#include "sorts/insertionSort.cpp"
#include "sorts/mergesort.cpp"
#include "sorts/quicksort.cpp"
#include "sorts/selectionSort.cpp"

class SortDriver {
    private:

        /*
        * @pre array was generated with the ArrayGenerator and size is valid
        * @post a new array is created by the Array Generator
        * @return pointer to a new array containing the values from the Array generator
        */
        static double* createArray(std::string orderType, int size);

        /*
        * @pre array was generated with the ArrayGenerator and size is valid
        * @post prints the array to console
        */
        static void printArray(double* arr, int size);

        /*
    	* @return true if order type and sorting type parameters are valid
        */
    	static bool areParametersValid(std::string orderParameter, std::string sortParameter);

        /*
        * @return true if sorting type parameter is valid
        */
        static bool isSortParameterValid(std::string sortParameter);

        /*
        * @return true if order type parameter is valid
        */
        static bool isOrderParameterValid(std::string orderParameter);

        /*
        * @pre valid sort type is passed in
        * @post sorts the array according to the sort algorithm queried
        * @return time taken to complete the sort
        */
        static double doSort(double* arr, std::string sortName, int size);

    public:

        /*
        * @pre arguments are valid
        * @post results have correct timing information
        */
        static void run(int argc, char** argv);

        /*
        * @pre arguments are valid
        * @post average results are saved to an output file
        */
        static void runAnalysis(int argc, char** argv);

        /*
        * @post prints results to console
        */
        static void printResults(int size, std::string sortName, std::string orderName, double timeTaken);

        /*
        * @post prints menu to console
        */
        static void printHelpMenu();
};

#endif
