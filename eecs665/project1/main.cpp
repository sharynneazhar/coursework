/**
 * Assignment 1 - NFA to DFA Converter
 * @author  Sharynne Azhar
 * @course  EECS 665 - Compiler Construction (Fall 2017)
 * @date    10-13-2016
 * @file    main.cpp
 * @desc    Converts an NFA from standard input into its equivalent DFA
 */

#include <iostream>
#include <sstream>
#include <queue>
#include <set>
#include <unordered_map>
#include <vector>

#include "Utils.h"

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

  // A map of NFA transitions states, key = state symbol, value = transitions
  std::unordered_map<std::string, std::unordered_map<int, std::set<int>>> nfa;


  ////////////////////////////////////////////////////
  //  Read NFA from standard input
  ////////////////////////////////////////////////////

  std::string line;
  std::string temp;
  std::stringstream stream;
  std::vector<std::string> input;

  while(std::getline(std::cin, line)) {
    input.push_back(line);
  }

  // Parse initial state of NFA from first line
  nfaInitialState = std::stoi(Utils::trimBrackets(input[0]));

  // Parse final states of NFA from second line
  // For example, {2,5} would return a set containing 2 and 5
  nfaFinalStates = Utils::splitStrToIntSet(Utils::trimBrackets(input[1]));

  // Parse the total number of states in NFA from the third line
  // ignore the words "Total States:" and get the actual number
  stream.str(input[2]);
  stream >> temp >> temp >> temp;
  numStates = std::stoi(temp);

  // Parse the state symbols from fourth line
  // ignore the word "State" in the front
  stream.clear();
  stream.str(input[3]);
  stream >> temp;
  while (stream >> temp) {
    stateSymbols.push_back(temp);
    numStateSymbols++;
  }

  // Parse the transition diagrams from the fifth line and on
  // Each line assumes the following structure: "1 	{}	{}	{2,5}", where the first number
  // is the state number and the bracketed numbers are the state transitions
  for (int i = 0; i < numStates; i++) {
    int stateNum;
    stream.clear();
    stream.str(input[i + 4]);

    // Get the state number
    stream >> stateNum;

    // Get the transitions for each state symbols for the current state
    for (std::size_t j = 0; j < stateSymbols.size(); j++) {
      if (stream >> temp) {
        // Create a transition set for the current state symbol
        std::set<int> transitionSet = Utils::splitStrToIntSet(Utils::trimBrackets(temp));

        // Create a pair of the current state and transitions
        std::pair<int, std::set<int>> stateTransitionPair(stateNum, transitionSet);

        // Insert into NFA
        // If the NFA does not contain the state symbol yet, create a key and insert
        if (nfa.find(stateSymbols[j]) == nfa.end()) {
          std::unordered_map<int, std::set<int>> nfaSymbolMap;
          nfa.insert(std::make_pair(stateSymbols[j], nfaSymbolMap));
        }

        // Otherwise, find the state symbol key and insert the transition pair
        nfa.find(stateSymbols[j])->second.insert(stateTransitionPair);
      }
    }
  }


  ////////////////////////////////////////////////////
  //  Variables for DFA
  ////////////////////////////////////////////////////

  std::unordered_map<std::string, std::unordered_map<int, int>> dfa;
  std::unordered_map<int, std::set<int>> newStates;
  std::queue<std::pair<int, std::set<int>>> queue;
  std::set<int> dfaFinalStates;
  std::set<int> tempSet;
  int dfaStateNum = 0;


  ////////////////////////////////////////////////////
  //  Convert NFA to DFA
  ////////////////////////////////////////////////////

  // Create a new map for the DFA
  for (std::size_t i = 0; i < stateSymbols.size() - 1; i++) {
    std::unordered_map<int, int> dfaSymbolMap;
    dfa.insert(std::make_pair(stateSymbols[i], dfaSymbolMap));
  }

  // Perform the E-Closure of the initial state
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

    // Perform the E-Closure
    for (std::size_t i = 0; i < stateSymbols.size(); i++) {
      if (stateSymbols[i] != "E") {
        tempSet = Utils::move(nfa.find(stateSymbols[i])->second, currClosure.second, stateSymbols[i]);

        // If the move produced results besides {}, find the E-closure of it
        if (!tempSet.empty()) {
          tempSet = Utils::eClosure(nfa.find("E")->second, tempSet);
          std::pair<int, int> stateTransititon;

          // Check if tempSet exists in current result set
          bool existingState = false;
          for (std::pair<int, std::set<int>> state : newStates) {
            // If the state already exists, add this transition to the DFA with same state number
            if (state.second == tempSet) {
              existingState = true;
              std::cout << state.first << "\n";
              stateTransititon = std::make_pair(currClosure.first, state.first);
              break;
            }
          }

          // If it is a new state, add it to the DFA with a new state number
          if (!existingState) {
            std::cout << dfaStateNum++ << "\n";
            std::pair<int, std::set<int>> newStateSetPair(dfaStateNum, tempSet);
            newStates.insert(newStateSetPair);
            queue.push(newStateSetPair);

            // If this new state set-pair contains a final state, add to DFA final state set
            for (int state : nfaFinalStates) {
              if (tempSet.find(state) != tempSet.end()) {
                dfaFinalStates.insert(dfaStateNum);
                break;
              }
            }

            // Add the transition to the DFA
            stateTransititon = std::make_pair(currClosure.first, dfaStateNum);
          }

          dfa.find(stateSymbols[i])->second.insert(stateTransititon);
        }
      }
    }
  }

  ////////////////////////////////////////////////////
  //  Print the DFA
  ////////////////////////////////////////////////////

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
    for (std::size_t j = 0; j < stateSymbols.size() - 1; j++) {
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
