/**
*	@file : main.cpp
*	@author : Sharynne Azhar
*	@date : 02-24-2016
*	@brief: Implementation file for Engine class program-
*/

#include "Engine.h"

void Engine::run(const std::string filename)
{
    readFile(filename, events);
    runService(events, vip, customer);
}

void Engine::readFile(const std::string filename, std::vector<std::string>& vec) throw (PrecondViolatedExcep)
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

        // convert all characters to uppercase for simpler checking
        std::string str;
        for (unsigned int i = 0; i < vec.size(); i++)
        {
            str = vec[i];
            for (unsigned int j = 0; j < str.size(); j++)
            {
                vec[i][j] = toupper(str[j]);
            }
        }
    }

    file.close();
}

bool Engine::isVip(const std::string name)
{
    // method checks out if customer's name start with a V
    while (name[0] != 'V' || (name[1] != 'I' && name[2] != 'P'))
    {
        return false;
    }

    return true;
}

void Engine::show(Stack<std::string>& stack, Queue<std::string>& queue)
{
    try
    {
        if (stack.isEmpty())
        {
            std::cout << queue.peekFront() << " is being served\n";
            std::cout << queue.getNext() << " is waiting in front of queue\n";
        }
        else
        {
            std::cout << stack.peek() << " is being served\n";
            std::cout << queue.peekFront() << " is waiting in front of queue\n";
        }
    }
    catch (PrecondViolatedExcep& e)
    {
        std::cout << e.what();
    }
}

void Engine::done(Stack<std::string>& stack, Queue<std::string>& queue)
{
    try
    {
        if (stack.isEmpty())
        {
            std::cout << queue.peekFront() << " is done\n";
            queue.dequeue();
            std::cout << queue.peekFront() << " is starting\n";
        }
        else
        {
            std::cout << stack.peek() << " is done\n";
            stack.pop();
            std::cout << stack.peek() << " is starting\n";
        }
    }
    catch (PrecondViolatedExcep& e)
    {
        std::cout << e.what();
    }
}

void Engine::runService(std::vector<std::string>& vec, Stack<std::string>& stack, Queue<std::string>& queue)
{
    std::cout << "\n=============================\n\n";
    std::cout << "  WELCOME TO LYDIA'S LATTE    \n";
    std::cout << "\n=============================\n\n";

    // with valid vector filled with contents of the file, break down sequence of events
    for (unsigned int i = 0; i < vec.size(); i++)
    {
        if (vec[i] == "SHOW"  && !queue.isEmpty())
        {
            show(stack, queue);
        }
        else if (vec[i] == "DONE" && !stack.isEmpty() && !queue.isEmpty())
        {
            done(stack, queue);
        }
        else
        {
            std::string temp = vec[i];
            if (isVip(temp))
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

    // last check to make sure stack and queue are empty
    while (!stack.isEmpty() || !queue.isEmpty())
    {
        done(stack, queue);
    }

    if (queue.isEmpty())
    {
        std::cout << "\nNo customers or VIPs left...\n";
        std::cout << "Lydia rests...\n";
    }
}
