/**
*	@file : Engine.h
*	@author : Sharynne Azhar
*	@date : 02-24-2016
*	@brief: Header file for engine class used as the simulation engine for main program
*/

#ifndef ENGINE_H
#define ENGINE_H

#include <iostream>
#include <fstream>
#include <string>
#include <vector>

#include "Stack.h"
#include "Queue.h"
#include "PrecondViolatedExcep.h"

class Engine
{
    private:
        std::vector<std::string> events;
        Queue<std::string> customer;
        Stack<std::string> vip;

    public:
        /*
        * @pre expects user provide a file to read
        * @post uses readFile() to create an vector of events and runService to execute
        */
        void run(const std::string file);

        /*
        * @pre a valid file and an instance of a vector are required
        * @post read file provided by user and inputs the contents into a vector
        * @throw error if user provides a bad file or empty file
        */
        void readFile(const std::string filename, std::vector<std::string>& vec) throw (PrecondViolatedExcep);

        /*
        * @pre a name of type string in passed in
        * @post determines if the customer is a VIP or otherwise
        * @return true if name matches "VIP(a number)" or "V(a number)"
        */
        bool isVip(const std::string name);

        /*
        * @pre an instance of a stack, and a queue are required
        * @post simulates the show process
        */
        void show(Stack<std::string>& stack, Queue<std::string>& queue);

        /*
        * @pre an instance of a stack, and a queue are required
        * @post simulates the done process
        */
        void done(Stack<std::string>& stack, Queue<std::string>& queue);

        /*
        * @pre an instance of a vector, a stack, and a queue are required
        * @post simulates the behavior of the program
        */
        void runService(std::vector<std::string>& vec, Stack<std::string>& stack, Queue<std::string>& queue);
};

#endif
