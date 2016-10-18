/**
*	@file : ArrayGenerator.cpp
*	@author : Sharynne Azhar
*	@date : 04-11-2016
*	@brief: Implementation file for ArrayGenerator class
*/

#include "ArrayGenerator.h"

void ArrayGenerator::ascending(double* arr, int size) {
    for (int i = 0; i < size; i++) {
        arr[i] = 0.0001 * static_cast<double>(i);
    }
}

void ArrayGenerator::descending(double* arr, int size) {
    for (int i = 0; i < size; i++) {
        arr[i] = 0.0001 * static_cast<double>(size - i - 1);
    }
}

void ArrayGenerator::random(double* arr, int size) {
    srand(time(NULL));
    for (int i = 0; i < size; i++) {
        arr[i] = drand48();
    }
}
