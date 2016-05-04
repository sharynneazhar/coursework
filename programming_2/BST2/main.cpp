/**
*	@file : main.cpp
*	@author : Sharynne Azhar
*	@date : 04-25-2016
*	@brief: Main module
*/

//https://github.com/bdrillard/eecs-268/tree/master/Lab_10

#include <iostream>
#include <string>

#include "Engine.h"
#include "Dictionary.h"

int main(int argc, char* argv[])
{
	std::cout << "\n=====================\n\n";
    std::cout << " English Dictionary\n";
    std::cout << "\n=====================\n\n";

	if (argc < 2) {
		std::cout << "Invalid number of arguments! Please provide a dictionary file\n\n";
		return 0;
	}

	Engine<Dictionary, std::string> engine;
	engine.createTree(argv[1]);
	engine.run();


	return 0;
}
