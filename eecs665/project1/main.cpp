/**
 * Project 1 - NFA to DFA
 * @author  Sharynne Azhar
 * @course  EECS 665 - Compiler Construction (Fall 2017)
 * @date    10-13-2016
 * @file    main.cpp
 * @desc    Converts an input NFA into its equivalent DFA
 */

#include <iostream>
#include <fstream>
#include <string>
#include <vector>

int main(int argc, char const *argv[]) {
  // Open the file for reading
  std::ifstream file;
  std::string filename;

  // If a user does not provide a file, use the example_input.txt file
  // Otherwise, take filename provided from console
  filename = (argc < 2) ? "example_input.txt" : argv[1];
  file.open(filename);

  // Read the file line by line into a vector
  std::string line;
  std::vector<std::string> vec;
  while(std::getline(file, line)) {
    std::cout << line << "\n";
  }

  std::cout << std::endl;
  return 0;
}
