/**
*	@file : Dictionary.cpp
*	@author : Sharynne Azhar
*	@date : 04-25-2016
*	@brief: Implementation file for Dictionary class
*/

#include "Dictionary.h"

Dictionary::Dictionary() : word(""), defn("") {}

Dictionary::Dictionary(std::string w, std::string d): word(w), defn(d) {}

std::string Dictionary::getWord() const {
    return word;
}

void Dictionary::setWord(std::string w) {
    word = w;
}

std::string Dictionary::getDefinition() const {
    return defn;
}

void Dictionary::setDefinition(std::string d) {
    defn = d;
}

bool Dictionary::operator<(const Dictionary& dict) const {
    return word < dict.word;
}

bool Dictionary::operator>(const Dictionary& dict) const {
    return word > dict.word;
}

bool Dictionary::operator==(const Dictionary& dict) const {
    return word == dict.word;
}

bool Dictionary::operator<(const std::string w) const {
    return word < w;
}

bool Dictionary::operator>(const std::string w) const {
    return word > w;
}

bool Dictionary::operator==(const std::string w) const {
    return word == w;
}
