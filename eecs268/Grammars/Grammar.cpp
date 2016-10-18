/**
*	@file : Grammar.cpp
*	@author : Sharynne Azhar
*	@date : 03-22-2016
*	Purpose: Implemnetation file for the Grammar class object
*/

#include "Grammar.h"

void Grammar::readFile(std::string filename) throw (PVE)
{
    xmlFile.open(filename);

    if (xmlFile.fail()) // check if file is valid
        throw PVE("Error. Bad file.");

    if (xmlFile.peek() == std::ifstream::traits_type::eof())
        throw PVE("Error. Empty file");

    std::string input;
    while (xmlFile >> input)
        vector.push_back(input);

    if (checkFileFormat())
        parse(vector);

    xmlFile.close();
}

bool Grammar::checkFileFormat()
{
    char c;
    int lineCount = 0;

    xmlFile.clear();
    xmlFile.seekg(0, ios::beg);
    while (xmlFile >> std::noskipws >> c)
    {
        std::string tag = "";
        if (c == '<')
        {
            while (xmlFile >> std::noskipws >> c)
            {
                if (c == '>')
                {
                    break;
                }
                else
                {
                    if (isspace(c))
                    {
                        std::cout << "ERROR: Invalid tag encountered at line " << lineCount + 1 << "\n";
                        return false;
                    }

                    tag += c;
                }
            }

            if (tag[0] != '/')
            {
                tagStack.push(tag);
            }
            else
            {
                if (tag.substr(1).compare(tagStack.peek()) == 0)
                {
                    tagStack.pop();
                }
                else
                {
                    std::cout << "ERROR: Unexpected closing tag <" << tag << "> at line " << lineCount + 1 << "\n";
                    return false;
                }
            }
        }

        if (c == '\n')
            lineCount++;
    }

    if (tagStack.isEmpty())
        return true;

    return false;
}

void Grammar::parse(std::vector<std::string> vector)
{
    for (unsigned int i = 0; i < vector.size(); i++)
    {
        if (vector[i] == "<tab>" || vector[i] == "<p>" || vector[i] == "<listItem>")
        {
            if (vector[i] == "<tab>")
            {
                while (vector[i] != "</tab>")
                {
                    i++;
                    if (vector[i] == "<listItem>")
                    {
                        if (vector[i] != "</tab>" && vector[i] != "</listItem>")
                        {
                            i++;
                            std::cout << "\t*" << vector[i] << "\n";
                        }
                        else
                        {
                            std::cout << "\n";
                        }
                    }
                }
            }
            else if (vector[i] == "<p>")
            {
                while (vector[i] != "</p>")
                {
                    i++;

                    if (vector[i] == "</p>")
                    {
                        std::cout << "\n";
                    }
                    else
                    {
                        std::cout << vector[i] << " ";
                    }
                }
            }
            else if (vector[i] == "<listItem>")
            {
                while (vector[i] != "</listItem>")
                {
                    i++;
                    if (vector[i] == "</listItem>")
                    {
                        std::cout << "\n";
                    }
                    else
                    {
                        std::cout << "*" << vector[i];
                    }
                }
            }
        }
        else
        {
            std::cout << vector[i] << " ";
        }
    }
}
