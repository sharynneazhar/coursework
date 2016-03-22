/**
*	@file : Postfix.h
*	@author : Sharynne Azhar
*	@date : 03-07-2016
*	@brief: Header file for the postfix class used to convert a postfix notation to infix
*/

#ifndef POSTFIX_H
#define POSTFIX_H

#include <iostream>
#include <string>

#include "Stack.h"
#include "PrecondViolatedExcep.h"

class Postfix
{
    private:
        Stack<std::string> stk;
        std::string infix;

    public:
        // checks if a character is an operator */+-
        bool isOperator(char ch);

        // checks if a character is a letter
        bool isAlpha(char ch);

        // converts a postfix to an infix expression and throws new appropriate messages
        void convert(const std::string str) throw (PrecondViolatedExcep);
};

#endif
