/**
*	@file : main.cpp
*	@author : Sharynne Azhar
*	@date : 02-22-2016
*	@brief: Implementation file for driver program-
*/

#include <iostream>
#include <fstream>
#include <string>
#include <vector>

#include "Stack.h"
#include "Queue.h"
#include "PrecondViolatedExcep.h"

/*
* @pre a valid file and an instance of a vector are required
* @post read file provided by user and inputs the contents into a vector
* @throw error if user provides a bad file or empty file
*/
void readFile(const std::string filename, std::vector<std::string>& vec) throw (PrecondViolatedExcep);

/*
* @pre an instance of a vector, a stack, and a queue are required
* @post simulates the behavior of the program
*/
void run(std::vector<std::string>& vec, Stack<std::string>& stack, Queue<std::string>& queue);

/*
* @pre a name of type string in passed in
* @post determines if the customer is a VIP or otherwise
* @return true if name matches "VIP(a number)" or "V(a number)"
*/
bool isVip(std::string name);

int main(int argc, char** argv)
{
    if (argc < 2)
    {
        // force user to provide a file containing sequence of events
        std::cout << "Please provide a transaction file\n";
        std::cout << "\nFor example: ./prog TransactionFile.txt\n\n";
    }
    else
    {
        std::string filename = argv[1];
        std::vector<std::string> events;
        Queue<std::string> customer;
        Stack<std::string> vip;

        try
        {
            readFile(filename, events);
            std::cout << "\n=============================\n\n";
        	std::cout << "  WELCOME TO LYDIA'S LATTE    \n";
        	std::cout << "\n=============================\n\n";
        }
        catch (PrecondViolatedExcep& e)
        {
            std::cerr << e.what();
        }

        run(events, vip, customer);
    }

    return 0;
}

void readFile(const std::string filename, std::vector<std::string>& vec) throw (PrecondViolatedExcep)
{
    std::ifstream file;
    file.open(filename);

    if (file.fail()) // check if file is valid
    {
        throw PrecondViolatedExcep("Bad file.\n");
    }
    else if (file.peek() == std::ifstream::traits_type::eof())
    {
        throw PrecondViolatedExcep("Empty file\n");
    }
    else
    {
        std::string temp;
        while (file >> temp)
        {
            vec.push_back(temp);
        }

        std::string str;
        for (unsigned int i = 0; i < vec.size(); i++)
        {
            str = vec[i];
            for (unsigned int j = 0; j < str.size(); j++)
            {
                // convert all characters to uppercase for simpler checking
                vec[i][j] = toupper(str[j]);
            }
        }
    }

    file.close();
}

void run(std::vector<std::string>& vec, Stack<std::string>& stack, Queue<std::string>& queue)
{
    // with valid vector filled with contents of the file, break down sequence of events
    for (unsigned int i = 0; i < vec.size(); i++)
    {
        if (vec[i] == "SHOW"  && !stack.isEmpty() && !queue.isEmpty())
        {
            try
            {
                if (!stack.isEmpty())
                {
                    std::cout << "### " << stack.peek() << " is being served\n";
                }
                else
                {
                    std::cout << "### " << queue.peekFront() << " is being served\n";
                }

                std::cout << "### " << queue.peekFront() << " is waiting in front of queue\n";
            }
            catch (PrecondViolatedExcep& e)
            {
                std::cout << e.what();
            }

        }
        else if (vec[i] == "DONE" && !stack.isEmpty() && !queue.isEmpty())
        {
            try
            {
                std::cout << "--> " << stack.peek() << " is done\n";
                stack.pop();
                std::cout << "<-- " << stack.peek() << " is starting\n";
            }
            catch (PrecondViolatedExcep& e)
            {
                std::cout << e.what();
            }

            try
            {
                if (stack.isEmpty())
                {
                    std::cout << "--> " << queue.peekFront() << " is done\n";
                    queue.dequeue();
                    std::cout << "<-- " << queue.peekFront() << " is starting\n";
                }
            }
            catch (PrecondViolatedExcep& e)
            {
                std::cout << e.what();
            }
        }
        else
        {
            std::string temp = vec[i];

            if (temp.length() == 4 && isVip(temp))
            {
                if (stack.isEmpty())
                {
                    stack.push(temp);
                    std::cout << stack.peek() << " arrives\n";
                }
                else if (temp > stack.peek())
                {
                    stack.push(temp);
                    std::cout << stack.peek() << " arrives\n";
                }
                else
                {
                    std::cout << temp << " avoids boss and leaves\n";
                }
            }

            if (!isVip(temp) && temp != "SHOW" && temp != "DONE")
            {
                queue.enqueue(temp);
                std::cout << temp << " arrives\n";
            }

        }
    }

    if (queue.isEmpty())
    {
        std::cout << "\nNo customers or VIPs left...\n";
        std::cout << "Lydia rests...\n";
    }
}

bool isVip(std::string name)
{
    while (name[0] != 'V' || (name[1] != 'I' && name[2] != 'P' && !isdigit(name[3])))
    {
        return false;
    }

    while (name[0] != 'V' && !std::isdigit(name[1]))
    {
        return false;
    }

    return true;
}
