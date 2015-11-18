/**
*	@file : MazeCreationException.cpp
*	@author : Sharynne Azhar
*	@date : 2015.11.12
*	@brief: Implementation file for MazeCreationException class
*/

#include "MazeCreationException.h"

MazeCreationException::MazeCreationException(const char* message) : std::runtime_error(message)
{

}
