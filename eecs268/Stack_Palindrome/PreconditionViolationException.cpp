/**
*	@file : PreconditionViolationException.cpp
*	@author : Sharynne Azhar
*	@date : 02-15-2016
*	Purpose: Implementation file for the PreconditionViolationException class
*/

#include "PreconditionViolationException.h"

PreconditionViolationException::PreconditionViolationException(const char* message) : std::runtime_error(message)
{
    // Empty constructor
}
