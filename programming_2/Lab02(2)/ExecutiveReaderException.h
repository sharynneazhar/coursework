/**
*	@file : ExecutiveReaderException.h
*	@author : Sharynne Azhar
*	@date : 02-01-2016
*	Purpose: header file for the ExecutiveReaderException class used to catch errors
*/

#ifndef EXECUTIVEREADEREXCEPTION_H
#define EXECUTIVEREADEREXCEPTION_H

#include <stdexcept>
#include <string>

class ExecutiveReaderException : public std::runtime_error
{
public:
    /**
    @post creates an exception with message
    */
    ExecutiveReaderException(const char* message);
};

#endif
