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

class Utils {
private:
  /**
   * Trims whitespace off a string
   * @param  str String to trim
   * @return     Trimmed string with no whitespace
   * @ref: https://stackoverflow.com/questions/216823/whats-the-best-way-to-trim-stdstring
   */

  // trim left end
  static std::string &leftTrim(std::string &str) {
    str.erase(str.begin(), std::find_if(str.begin(), str.end(), std::not1(std::ptr_fun<int, int>(std::isspace))));
    return str;
  }

  // trim right end
  static std::string &rightTrim(std::string &str) {
    str.erase(std::find_if(str.rbegin(), str.rend(), std::not1(std::ptr_fun<int, int>(std::isspace))).base(), str.end());
    return str;
  }

public:
  /**
   * Constructor
   */
  Utils() {}

  // Trims whitespace
  static std::string &trim(std::string &s) {
    return rightTrim(leftTrim(s));
  }

  // Finds a substring in a string contained within two specified delimiters
  std::string findString(std::string str, std::string startDelimiter, std::string stopDelimiter) {
    std::size_t startIndex = str.find(startDelimiter);
    std::size_t stopIndex = str.find(stopDelimiter);
    std::string target = str.substr(startIndex + 1, startIndex - stopIndex - 1);
    return trim(target);
  }

  // Finds a substring in a string contained within one specified delimiter
  std::string findString(std::string str, std::string startDelimiter) {
    std::size_t startIndex = str.find(startDelimiter);
    std::size_t stopIndex = str.length();
    std::string target = str.substr(startIndex + 1, startIndex - stopIndex - 1);
    return trim(target);
  }

  /// Splits a string by delimiter
  // @ref: https://stackoverflow.com/questions/14265581/parse-split-a-string-in-c-using-string-delimiter-standard-c
  std::vector<std::string> splitString(std::string str, std::string delimiter) {
    std::vector<std::string> vec;
    std::size_t index = 0;
    while ((index = str.find(delimiter)) != std::string::npos) {
      vec.push_back(str.substr(0, index));
      str.erase(0, index + delimiter.length());
    }
    vec.push_back(str);
    return vec;
  }

  // Converts a string vector to int vector type
  std::vector<int> stoiVec(std::vector<std::string> strVec) {
    std::vector<int> intVec;
    for (std::size_t i = 0; i < strVec.size(); i++) {
      intVec.push_back(std::stoi(strVec[i]));
    }
    return intVec;
  }

  // Prints out all the elements in a vector
  void printVec(std::vector<int> vec){
    std::cout <<  "{";
    for (std::size_t i = 0; i < vec.size(); i++) {
      std::cout << vec[i];
      if (i + 1 != vec.size())
        std::cout << ", ";
      else
        std::cout << "}";
    }
  }

};

#endif
