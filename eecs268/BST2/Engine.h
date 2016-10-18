/**
*	@file : Engine.h
*	@author : Sharynne Azhar
*	@date : 04-25-2016
*	@brief: Header file for Engine class used to read the dictionary file and load a BST with entries
*/

#ifndef ENGINE_H
#define ENGINE_H

#include <iostream>
#include <fstream>
#include <string>

#include "BinarySearchTree.h"
#include "Dictionary.h"
#include "NotFoundException.h"

bool isTreeCopied = false;

std::istream& operator>>(std::istream& is, Dictionary& dict);
std::ostream& operator<<(std::ostream& os, const Dictionary& dict);
void visit(std::string& aKey);
void visit(Dictionary& anItem);

template<typename ItemType, typename KeyType>
class Engine
{
    private:
        BinarySearchTree<Dictionary, std::string> bst;
        BinarySearchTree<Dictionary, std::string>* copyBst;

        std::string makeUpper(std::string str);
        void printMenu();
        void saveMenu(int order, int tree, ofstream& outFile);

        void testAdds(BinarySearchTree<Dictionary, std::string> dictionary);
        void testRemoves(BinarySearchTree<Dictionary, std::string> dictionary);
        void testWriteToFile(BinarySearchTree<Dictionary, std::string> dictionary);

    public:
        Engine();
       ~Engine();

        void createTree(std::string filename);
        void run();
};

#include "Engine.cpp"

#endif
