/**
 * @author  Sharynne Azhar
 * @course  EECS 665 - Compiler Construction (Fall 2017)
 * @date    10-13-2016
 * @file    Utils.cpp
 * @desc    Contains any helper functions used in the main program
 */

#ifndef UTILS_H
#define UTILS_H

#include <iostream>
#include <string>
#include <algorithm>
#include <functional>
#include <cctype>
#include <locale>
#include <vector>
#include <sstream>
#include <queue>
#include <set>
#include <unordered_map>

class Utils {
private:
  // Trims whitespace on left end of string
  static std::string &leftTrim(std::string &str) {
    str.erase(str.begin(), std::find_if(str.begin(), str.end(), std::not1(std::ptr_fun<int, int>(std::isspace))));
    return str;
  }

  // Trims whitespace on right end of string
  static std::string &rightTrim(std::string &str) {
    str.erase(std::find_if(str.rbegin(), str.rend(), std::not1(std::ptr_fun<int, int>(std::isspace))).base(), str.end());
    return str;
  }

  static void eClosureHelper(std::unordered_map<int, std::set<int>> transitions, std::set<int> states, std::set<int> &result) {
    for (int state : states) {
      if (result.find(state) == result.end()) {
        result.insert(state);
        eClosureHelper(transitions, transitions.find(state)->second, result);
      }
    }
  }

public:
  // Constructor
  Utils() {}

  // Trims whitespace
  // @ref: https://stackoverflow.com/questions/216823/whats-the-best-way-to-trim-stdstring
  static std::string &trim(std::string &s) {
    return rightTrim(leftTrim(s));
  }

  // Finds a substring in a string contained within two specified delimiters
  static std::string trimBrackets(std::string str) {
    std::string target = str.substr(str.find("{") + 1, str.find("}") - str.find("{") - 1);
    return trim(target);
  }

  // Finds a substring in a string contained within one specified delimiter
  static std::string getSubstr(std::string str, std::string delimiter) {
    std::size_t startIndex = str.find(delimiter);
    std::size_t stopIndex = str.length();
    std::string target = str.substr(startIndex + 1, startIndex - stopIndex - 1);
    return trim(target);
  }

  // Splits a comma separated string and returns a set of corresponding integer values
  // For example, a string "1,2,3" would return a set of integers containing 1, 2, and 3
  static std::set<int> splitStrToIntSet(std::string str) {
    std::set<int> aSet;
    int temp;

    while (!str.empty()) {
      if (str.find(",") != std::string::npos) {
        temp = std::stoi(str.substr(str.find_last_of(",") + 1));
        str.erase(str.find_last_of(","));
      } else {
        temp = std::stoi(str);
        str = "";
      }

      aSet.insert(temp);
    }

    return aSet;
  }

  // Parses the transition matrix to determine the states connected to this state by the epsilon transition.
  static std::set<int> eClosure(std::unordered_map<int, std::set<int>> transitions, std::set<int> states) {
    std::set<int> result;
    eClosureHelper(transitions, states, result);
    std::cout << "E-closure{";
    printSet(states);
    std::cout << "} = {";
    printSet(result);
    std::cout << "} = ";
    return result;
  }

  // Performs the move function for a certain state set
  static std::set<int> move(std::unordered_map<int, std::set<int>> transitions, std::set<int> states, std::string input) {
      std::set<int> result, transition;

      for (int state : states) {
        transition = transitions.find(state)->second;
        result.insert(transition.begin(), transition.end());
      }

      if (!result.empty()) {
        std::cout << "{";
        printSet(states);
        std::cout << "} --" << input << "--> {";
        printSet(result);
        std::cout << "}\n";
      }

      return result;
  }

  // Prints out all the elements in a vector
  static void printSet(std::set<int> set) {
    std::size_t i = 0;
    for (int s : set) {
      std::cout << s;
      if (i != set.size() - 1)
        std::cout << ",";
      i++;
    }
  }


};

#endif
