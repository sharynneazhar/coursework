/**
 * Assignment 1 - NFA to DFA Converter
 * @author  Sharynne Azhar
 * @date    10-13-2016
 * @file    Converter.h
 * @desc    Converts an input NFA into its equivalent DFA
 */

#ifndef CONVERTER_H
#define CONVERTER_H

#include <iostream>
#include <string>
#include <fstream>
#include <vector>
#include <algorithm>
#include <sstream>
#include <cctype>

#include "Utils.h"

class Converter {
  private:
    std::vector<int> epsilonClosure(std::vector<int> &, int);
    void mark(std::vector<int>, int);
    void getDFAInfo();

  public:
    Utils utils;

    int initialState = 0;
    int dfaFinalStates = 0;
    int numStates = 0;
    int numStateSymbols = 0;

    std::vector<std::string> inputVector;
    std::vector<int> finalStates;
    std::vector<char> stateSymbols;
    std::vector<std::string> nextTransitions;
    std::vector<int> intNextTransitions;
    std::vector<std::vector<int>> epsilonStates;
    std::vector<std::vector<int>> dfaStates;
    std::vector<int> dfaFinalVector;
    std::vector<int> dfaTransitionNum;

  Converter(std::vector<std::string> vec);
  ~Converter();

  void setup();
  void load();
  void convert();
  void printResults();
};

#include "converter.cpp"

#endif
