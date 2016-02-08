/**
*	@file : Sorts.h
*	@author : Sharynne Azhar
*	@date : 2015.10.26
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
#include <fstream>

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
    * @post sorts arr using merge sort
    * @return none
    */
    static void mergeSort(T arr[], int size);

    /*
    * @pre array with valid size exists
    * @post sorts arr using quickSort with median flag set to false
    * @return none
    */
    static void quickSort(T arr[], int size);

    /*
    * @pre array with valid size exists
    * @post sorts arr using quickSortRec with median flag set to true
    * @return none
    */
    static void quickSortWithMedian(T arr[], int size);

    /*
    * @pre array with valid size exists
    * @post none
    * @return true if arr is in ascending order, false otherwise
    */
    static bool isSorted(T arr[], int size);

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

private:
    /*
    * @pre two sorted arrays with valid sizes
    * @post used by merge sort to two sorted combine arrays into a single sorted array
    * @return none
    */
    static void merge(T* a1, T* a2, int size1, int size2);

    /*
    * @pre array with valid size exists
    * @post sorts the array by partitioning the array, quick sorting every left of the pivot, and quick sorting everything right of the pivot; passes median to partition to inform that method how to pick a pivot value
    * @return none
    */
    static void quickSortRec(T arr[], int first, int last, bool median);

    /*
    * @pre array with valid size exists
    * @post puts the median value in the last position
    * @return none
    */
    static void setMedianPivot(T arr[], int first, int last);

    /*
    * @pre array with valid size exists
    * @post divides the array and chooses a pivot based on the median flag
    * @return the index of the pivot
    */
    static int partition(T arr[], int first, int last, bool median);

    /*
    * @pre array with valid size exists
    * @post indices of sorted arr shuffled into random order
    * @return none
    */
    static void shuffle(T arr[], int size);

    /*
    * @pre an array exists
    * @post swaps value of a and b
    * @return none
    */
    static void swap(T arr[], int a, int b);
};
#include "Sorts.hpp"
#endif
