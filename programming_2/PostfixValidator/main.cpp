/**
*	@file : main.cpp
*	@author : Sharynne Azhar
*	@date : 02-29-2016
*	Purpose: main driver
*/

#include <iostream>
#include <string>

#include "Stack.h"
#include "PrecondViolatedExcep.h"

// checks if a character is an operator */+-
bool isOperator(char ch);

// checks if a character is a letter
bool isAlpha(char ch);

// parses a string to check for illegal input
bool parse(std::string str);

// converts a postfix to an infix expression
void postfixToInfix(std::string str);

int main() {

    std::cout << "\n===============================\n\n";
    std::cout << " POSTFIX EXPRESSION VALIDATOR    \n";
    std::cout << "\n===============================\n\n";

    // get input string from user
    std::string input;
    do {
        std::cout << "Enter a string: ";
        std::getline(cin, input);
        if (parse(input)) {
            try {
                postfixToInfix(input);
            }
            catch (PrecondViolatedExcep& e) {
                std::cout << e.what();
            }
        }
    } while (input != "#");

    return 0;
}

bool isOperator(char ch) {
    if (ch == '*' || ch == '/' || ch == '+' || ch == '-') {
        return true;
    }

    return false;
}

bool isAlpha(char ch) {
    if (ch >= 'a' && ch <= 'z') return true;
    if (ch >= 'A' && ch <= 'Z') return true;

    return false;
}

bool parse(std::string str) {
    char temp;
    for (unsigned int i = 0; i < str.length(); i++) {
        temp = str[i];
        if (!isOperator(temp) && !isAlpha(temp)) {
            if (temp == '#')
                std::cout << "\nExiting...\n\n";
            else
                std::cout << "\nInvalid postfix string: encountered an illegal character\n\n";
                return false;
        }
    }

    return true;
}

void postfixToInfix(std::string str) {
    Stack<std::string> parsedStack;
    std::string postfix = "";
    std::string temp = "";
    std::string temp2 = "";

    // parse each character from left to right
    for (unsigned int i = 0; i < str.length(); i++) {
        if (isOperator(str[i])) {
            // get righthand operand
            temp = parsedStack.peek();
            parsedStack.pop();

            // get lefthand operand
            temp2 = parsedStack.peek();
            parsedStack.pop();

            // concatenate into a string and push to stack
            postfix = "(" + temp2 + str[i] + temp + ")";
            parsedStack.push(postfix);
        }

        if (isAlpha(str[i])) {
            temp = str[i];
            parsedStack.push(temp);
        }
    }

    std::cout << "\nThe equivalent infix is " << postfix << "\n\n";
}
