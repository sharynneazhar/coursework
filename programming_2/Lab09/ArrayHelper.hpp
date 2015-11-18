/**
*	@file ArrayHelper.cpp
*	@author John Gibbons
*	@date 2014.04.19
*
*/

#include "ArrayHelper.h"

template <typename T>
void ArrayHelper<T>::print1DArray(const T* arr, int size, std::string delimeter)
{
	for(int i=0; i<size; i++)
	{
		std::cout << arr[i];
		if(i<size-1)
		{	
			std::cout << delimeter;
		}
	}
	std::cout << std::endl;
}

template <typename T>
void ArrayHelper<T>::print2DArray(const T* const* arr, int rows, int cols, std::string delimeter)
{
	for(int i=0; i<rows; i++)
	{
		ArrayHelper<T>::print1DArray(arr[i], cols, delimeter);
	}
}

template <typename T>
bool ArrayHelper<T>::areArraysEqual(const T* arr1, int size1, const T* arr2, int size2)
{
	if(size1 != size2)
	{
		return(false);
	}
	else
	{
		for(int i=0; i<size1; i++)
		{
			if(arr1[i] != arr2[i])
			{
				return(false);
			}
		}
	}

	return(true);
}


template <typename T>
bool ArrayHelper<T>::areArraysEqual(const T* const* arr1, int rows1, int cols1, const T* const* arr2, int rows2, int cols2)
{
	if(rows1 != rows2 || cols1 != cols2)
	{
		return(false);
	}
	else
	{
		for(int i=0; i<rows1; i++)
		{
			if(!ArrayHelper::areArraysEqual(arr1[i], cols1, arr2[i], cols2))
			{
				return(false);
			}
		}
	}

	return(true);
}
