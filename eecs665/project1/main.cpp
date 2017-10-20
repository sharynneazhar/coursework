/**
 * Assignment 1 - NFA to DFA Converter
 * @author  Sharynne Azhar
 * @course  EECS 665 - Compiler Construction (Fall 2017)
 * @date    10-13-2016
 * @file    main.cpp
 * @desc    Converts an input NFA into its equivalent DFA
 *
 * NOTE: The program assumes that input follows the following structure:
 * Initial State: {1}
 * Final States: {11}
 * Total States: 11
 * State a b E
 * 1 {} {} {2,5}
 * 2 {3} {} {}
 * 3 {} {4} {}
 * ...
 */

#include <iostream>
#include "Converter.h"

int main(int argc, char** argv){
  std::cout << "\n===================================";
  std::cout << "\n=====   NFA to DFA Converter  =====";
  std::cout << "\n===================================\n";

  // Open the file for reading
  std::ifstream file(argv[1]);
  std::vector<std::string> input;
  std::string line;

  // Read the file line by line into a vector
  while(std::getline(std::cin, line)) {
    input.push_back(line);
  }

  Converter converter(input);
  converter.setup();
  converter.convert();
  converter.printResults();
}
