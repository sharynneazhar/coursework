/**
*	@file ArrayPrinter.h
*	@author John Gibbons
*	@date 2014.04.19
*
*/


#ifndef ARRAY_HELPER_H
#define ARRAY_HELPER_H

#include <string>
#include <iostream>

template <typename T>
class ArrayHelper
{
	public:
	/**
	*	@pre arr is a pointer to an array of the given size
	*	@post the array is printed using std::cout, each value separated by the given delimeter
	*		The arrays are unchanged.
	*/
	static void print1DArray(const T* arr, int size, std::string delimeter);


	/**
	*	@pre arr is an array of the given size
	*	@post the array is printed using std::cout, each value separated by the given delimeter
	*		The arrays are unchanged.
	*/
	static void print2DArray(const T* const* arr, int rows, int cols, std::string delimeter);

	/**
	*	@pre arr1 and arr 2 are arrays of the given size. T is comparable with == 
	*	@post the array is printed using std::cout, each value separated by the given delimeter.
	*		The arrays are unchanged.
	*/
	static bool areArraysEqual(const T* arr1, int size1, const T* arr2, int size2);


	/**
	*	@pre arr1 and arr 2 are arrays of the given dimensions. T is comparable with == 
	*	@post None. The arrays are unchanged.
	*	@return trues if the arrays are equal in dimension and each element is equal
	*/
	static bool areArraysEqual(const T* const* arr1, int rows1, int cols1, const T* const* arr2, int rows2, int cols2);
};

#include "ArrayHelper.hpp"

#endif

