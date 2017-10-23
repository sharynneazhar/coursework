/**
 * Assignment 1 - NFA to DFA Converter
 * @author  Sharynne Azhar
 * @course  EECS 665 - Compiler Construction (Fall 2017)
 * @date    10-13-2016
 * @file    main.cpp
 * @desc    Converts an input NFA into its equivalent DFA
 */

#include <iostream>
#include <sstream>
#include <queue>
#include <set>
#include <unordered_map>
#include <vector>

#include "Utils.h"

#define DEBUG 1

int main(int argc, char** argv){
  std::cout << "\n===================================";
  std::cout << "\n=====   NFA to DFA Converter  =====";
  std::cout << "\n===================================\n\n";


  ////////////////////////////////////////////////////
  //  Variables for NFA Information
  ////////////////////////////////////////////////////

  int nfaInitialState = 0;
  int numStates = 0;
  int numStateSymbols = 0;
  std::set<int> nfaFinalStates;
  std::vector<std::string> stateSymbols;
  std::unordered_map<std::string, std::unordered_map<int, std::set<int>>> nfa;


  ////////////////////////////////////////////////////
  //  Read NFA from standard input
  ////////////////////////////////////////////////////

  std::string line;
  std::string temp;
  std::vector<std::string> input;

  while(std::getline(std::cin, line)) {
    input.push_back(line);
  }

  // Parse initial state of NFA from first line
  nfaInitialState = std::stoi(Utils::trimBrackets(input[0]));

  // Parse final states of NFA from second line
  nfaFinalStates = Utils::splitStrToInt(Utils::trimBrackets(input[1]));

  // Parse the total number of states in NFA from the third line
  // ignore the words "Total States:" and get the actual number
  std::stringstream ss1(input[2]);
  ss1 >> temp >> temp >> temp;
  numStates = std::stoi(temp);

  // Parse the state symbols from fourth line
  // ignore the word "State" in the front
  std::stringstream ss2(input[3]);
  ss2 >> temp;
  while (ss2 >> temp) {
    stateSymbols.push_back(temp);
    numStateSymbols++;
  }

  // Parse the transition diagrams from the fifth line and on
  for (int i = 0; i < numStates; i++) {
    int stateNum;
    std::stringstream ss3(input[i + 4]);

    // Get the state number
    ss3 >> stateNum;

    // Get the transitions for the current state
    for (std::size_t j = 0; j < stateSymbols.size(); j++) {
      if (ss3 >> temp) {
        // Get the transition for the current state symbol
        std::set<int> transitions = Utils::splitStrToInt(Utils::trimBrackets(temp));

        // Create a pair with the current state number and transitions
        std::pair<int, std::set<int>> stateTransitionPairs(stateNum, transitions);

        if (nfa.find(stateSymbols[j]) == nfa.end()) {
          std::unordered_map<int, std::set<int>> nfaSymbolMap;
          nfa.insert(std::make_pair(stateSymbols[j], nfaSymbolMap));
        }

        nfa.find(stateSymbols[j])->second.insert(stateTransitionPairs);
      }
    }
  }


  ////////////////////////////////////////////////////
  //  Variables for DFA
  ////////////////////////////////////////////////////

  std::unordered_map<std::string, std::unordered_map<int, int>> dfa;
  std::unordered_map<int, std::set<int>> resultingStates;
  std::queue<std::pair<int, std::set<int>>> queue;
  std::set<int> dfaFinalStates;
  std::set<int> tempSet;
  int dfaStateNum = 0;


  ////////////////////////////////////////////////////
  //  Convert NFA to DFA
  ////////////////////////////////////////////////////

  // Insert an empty unordered_map for each state symbol in the NFA to the DFA
  for (std::size_t i = 0; i < stateSymbols.size() - 1; i++) {
    std::unordered_map<int, int> dfaSymbolMap;
    dfa.insert(std::make_pair(stateSymbols[i], dfaSymbolMap));
  }

  // Find the E-Closure of the initial state
  tempSet.insert(nfaInitialState);
  tempSet = Utils::eClosure(nfa.find("E")->second, tempSet); dfaStateNum++;
  std::cout << dfaStateNum << std::endl;

  // Add the E-Closure transition and DFA state to a queue
  queue.push(std::make_pair(dfaStateNum, tempSet));

  // Mark every state currently in the queue
  while (!queue.empty()) {
    // Get the next E-Closure to process
    std::pair<int, std::set<int>> currClosure = queue.front(); queue.pop();
    std::cout << "\nMark " << currClosure.first << std::endl;

    for (std::size_t i = 0; i < stateSymbols.size(); i++) {
      if (stateSymbols[i] != "E") {
        tempSet = Utils::move(nfa.find(stateSymbols[i])->second, currClosure.second, stateSymbols[i]);

        // If the move produced results besides {}, find the E-closure of it
        if (!tempSet.empty()) {
          tempSet = Utils::eClosure(nfa.find("E")->second, tempSet);
          std::pair<int, int> stateTransititon;

          // Check if tempSet exists in current result set
          bool stateExists = false;
          for (std::pair<int, std::set<int>> state : resultingStates) {
            if (state.second == tempSet) {
              stateExists = true;

              // Finish printing the 'E-closure' line
              std::cout << state.first << "\n";

              // Add this connection to the dfa results
              stateTransititon = std::make_pair(currClosure.first, state.first);
              break;
            }
          }

          // If we encounter a new state of the dfa, add it to the dfa data structure
          if (!stateExists) {
            // Finish printing the 'E-closure' line
            dfaStateNum++;
            std::cout << dfaStateNum << "\n";
            std::pair<int, std::set<int>> newResultState(dfaStateNum, tempSet);
            resultingStates.insert(newResultState);
            queue.push(newResultState);

            // If this new state contains a final state, add this to dfa final states
            for (int state : nfaFinalStates) {
              if (tempSet.find(state) != tempSet.end()) {
                dfaFinalStates.insert(dfaStateNum);
                break;
              }
            }

            // Add this connection to the dfa results
            stateTransititon = std::make_pair(currClosure.first, dfaStateNum);
          }

          dfa.find(stateSymbols[i])->second.insert(stateTransititon);
        }
      }
    }
  }

  std::cout << "\nInitial State: {" << nfaInitialState << "}\n";
  std::cout << "Final states: {";
  Utils::printSet(dfaFinalStates);
  std::cout << "}\n";
  std::cout << "State\t";

  for (std::size_t i = 0; i < stateSymbols.size() - 1; i++) {
    std::cout << stateSymbols[i] << "\t";
  }

  for (int i = 1; i <= dfaStateNum; i++) {
    std::cout << "\n" << i << "\t";

    for (int j = 0; j < stateSymbols.size() - 1; j++) {
      std::unordered_map<int, int> dfaTransition = dfa.find(stateSymbols[j])->second;
      if (dfaTransition.find(i) == dfaTransition.end()) {
          std::cout << "{}\t";
      } else {
          int stateData = dfaTransition.find(i)->second;
          std::cout << "{" << stateData << "}";
          std::cout << "\t";
      }
    }
  }

  std::cout << std::endl;

  return 0;
}
