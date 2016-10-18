/**
*	@file : Dictionary.h
*	@author : Sharynne Azhar
*	@date : 04-25-2016
*	@brief: Header file for Dictionary class for searching entries and storing overloaded operators
*/

#ifndef DICTIONARY_H
#define DICTIONARY_H

#include <iostream>
#include <istream>
#include <fstream>
#include <string>

class Dictionary
{
    private:
    	std::string word;
    	std::string defn;

    public:
        Dictionary();
        Dictionary(std::string w, std::string d);

        std::string getWord() const;
        void setWord(std::string w);

        std::string getDefinition() const;
        void setDefinition(std::string d);

        bool operator<(const Dictionary& dict) const;
        bool operator>(const Dictionary& dict) const;
        bool operator==(const Dictionary& dict) const;
        bool operator<(const std::string w) const;
        bool operator>(const std::string w) const;
        bool operator==(const std::string w) const;

};

#endif
