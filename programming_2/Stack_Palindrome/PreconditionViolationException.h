/**
*	@file : PreconditionViolationException.h
*	@author : Sharynne Azhar
*	@date : 02-15-2016
*	Purpose: Header file of the PreconditionViolationException class used for error handling
*/

#ifndef PRECONDITIONVIOLATIONEXCEPTION_H
#define PRECONDITIONVIOLATIONEXCEPTION_H

#include <stdexcept>

class PreconditionViolationException : public std::runtime_error
{
    public:
        /**
        @post creates an exception with message
        */
        PreconditionViolationException(const char* message);
};

#endif
