/**
*	@file : Sorts.h
*	@author : Sharynne Azhar
*	@date : 2015.10.19
*	@brief: Header file for templated sort class used to implemetn sort algorithms.
*/

#ifndef SORTS_H
#define SORTS_H

#include <string>
#include <random>
#include <ctime>
#include <chrono>
#include <cassert>
#include <functional>

template <typename T>
class Sorts
{
public:
    /*
    * @pre array with valid size exists
    * @post sorts arr using bubble sort
    * @return none
    */
    static void bubbleSort(T arr[], int size);

    /*
    * @pre array with valid size exists
    * @post sorts arr using bogo sort
    * @return none
    */
    static void bogoSort(T arr[], int size);

    /*
    * @pre array with valid size exists
    * @post sorts arr using insertion sort
    * @return none
    */
    static void insertionSort(T arr[], int size);

    /*
    * @pre array with valid size exists
    * @post sorts arr using selection sort
    * @return none
    */
    static void selectionSort(T arr[], int size);

    /*
    * @pre array with valid size exists
    * @post none
    * @return true if arr is in ascending order, false otherwise
    */
    static bool isSorted(T arr[], int size);

    /*
    * @pre array with valid size exists
    * @post indices of sorted arr shuffled into random order
    * @return none
    */
    static void shuffle(T arr[], int size, std::default_random_engine& generator);

    /*
    * @pre size is not negative
    * @post creates new array with values within min and max values
    * @return pointer to first index of array
    */
    static int* createTestArray(int size, int min, int max);

    /*
    * @pre none
    * @post arr is sorted in ascending order
    * @return time in seconds, the sort required to sort arr
    */
    static double sortTimer(std::function<void(T[],int)> sort, T arr[], int size);

    /*
    * @pre an array exists
    * @post swaps value of a and b
    * @return none
    */
    static void swap(T arr[], int a, int b);

};
#include "Sorts.hpp"
#endif
