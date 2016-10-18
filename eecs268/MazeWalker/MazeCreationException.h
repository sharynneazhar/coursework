/**
*	@file : MazeCreationException.h
*	@author : Sharynne Azhar
*	@date : 2015.11.12
*	@brief: Header file for MazeCreationException class
*/

#ifndef MAZECREATIONEXCEPTION_H
#define MAZECREATIONEXCEPTION_H

#include <stdexcept>
#include <string>

class MazeCreationException : public std::runtime_error
{
public:
      /**
      * @post Creates an exception with the message
      */
      MazeCreationException(const char* message);
};

#endif
