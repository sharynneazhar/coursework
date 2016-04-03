/**
*	@file : BoardCreationException.cpp
*	@author : Sharynne Azhar
*	@date : 03-07-2016
*	Purpose: Implementation file for BoardCreationException class
*/

#include "BoardCreationException.h"

BoardCreationException::BoardCreationException(const char* message) : std::runtime_error(message) {}
