/**
*	@file : PreconditionViolationException.cpp
*	@author : Sharynne Azhar
*	@date : 2015.10.05
*	@brief: Implementation file for exception class
*/

#include "PreconditionViolationException.h"
#include <stdexcept>

PreconditionViolationException::PreconditionViolationException(const char* msg) : std::runtime_error(msg)
{
}
