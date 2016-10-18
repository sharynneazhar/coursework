/**
*	@file : PreconditionViolationException.h
*	@author : Sharynne Azhar
*	@date : 2015.10.05
*	@brief: Implementation file for PreconditionViolationException class to catch errors
*/

#ifndef PRECONDITIONVIOLATIONEXCEPTION_H
#define PRECONDITIONVIOLATIONEXCEPTION_H

#include <stdexcept>
#include <string>

class PreconditionViolationException : public std::runtime_error
{
public:
    PreconditionViolationException(const char* msg);
};

#endif
