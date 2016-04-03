/**
*	@file : BoardCreationException.h
*	@author : Sharynne Azhar
*	@date : 03-07-2016
*	Purpose: Header file for BoardCreationException class
*/

#ifndef BOARDCREATIONEXCEPTION_H
#define BOARDCREATIONEXCEPTION_H

#include <stdexcept>
#include <string>

class BoardCreationException : public std::runtime_error
{
    public:
          /**
          * @post Creates an exception with the message
          */
          BoardCreationException(const char* message);
};

#endif
