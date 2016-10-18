/**
*	@file : ExecutiveReaderException.cpp
*	@author : Sharynne Azhar
*	@date : 02-01-2016
*	Purpose: Implementation file for the ExecutiveReaderException class
*/

#include "ExecutiveReaderException.h"

ExecutiveReaderException::ExecutiveReaderException(const char* message) : std::runtime_error(message)
{

}
