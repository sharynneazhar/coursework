/**
*	@file : Grammar.h
*	@author : Sharynne Azhar
*	@date : 03-22-2016
*	Purpose: Header file for the Grammar class object used to validate any given BNF
*/

#ifndef GRAMMAR_H
#define GRAMMAR_H

#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <cctype>

#include "Stack.h"
#include "PVE.h"

class Grammar
{
    private:
        std::ifstream xmlFile;
        Stack<std::string> tagStack;
        std::vector<std::string> vector;

        bool checkFileFormat();

    public:
        void readFile(std::string filename) throw (PVE);

        void parse(std::vector<std::string> vector);

};

#endif
