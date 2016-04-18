/**
*	@file : ArrayGenerator.h
*	@author : Sharynne Azhar
*	@date : 04-11-2016
*	@brief: Header file for ArrayGenerator class used to generate an array for sorting
*/

#ifndef ARRAYGENERATOR_H
#define ARRAYGENERATOR_H

#include <stdlib.h>
#include <time.h>

class ArrayGenerator {
    public:

        /*
    	* @pre arr is declared with size greater than zero
    	* @post array of give size is filled with numbers in ascending order.
    	*/
        static void ascending(double* arr, int size);

        /*
    	* @pre arr[] is declared with size greater than zero
    	* @post array of give size is filled with numbers in descending order.
    	*/
        static void descending(double* arr, int size);

        /*
    	* @pre arr[] is declared with size greater than zero
    	* @post array of give size is filled with numbers in random order.
    	*/
        static void random(double* arr, int size);
};

#endif
