/**
*	@file : Engine.cpp
*	@author : Sharynne Azhar
*	@date : 04-25-2016
*	@brief: Implementation file for Engine class
*/

#include "Engine.h"

/////////////////////////////////////////////////
//      Global functions Section
/////////////////////////////////////////////////

std::istream& operator>>(std::istream& is, Dictionary& dict) {
    std::string word;
    std::string definition;

    is >> word;
    getline(is, definition);

    dict.setWord(word);
    dict.setDefinition(definition);

    return is;
}

std::ostream& operator<<(std::ostream& os, const Dictionary& dict) {
    os << dict.getWord() << ": " << dict.getDefinition() << "\n";
    return os;
}

void visit(std::string& aKey) {
    std::cout << aKey << "\n";
}

void visit(Dictionary& anItem) {
    std::cout << anItem << "\n";
}

/////////////////////////////////////////////////
//     Constructor & Destructor Section
/////////////////////////////////////////////////
template<typename ItemType, typename KeyType>
Engine<ItemType,KeyType>::Engine(){}

template<typename ItemType, typename KeyType>
Engine<ItemType,KeyType>::~Engine(){}

/////////////////////////////////////////////////
//     Helper methods Section
/////////////////////////////////////////////////
template<typename ItemType, typename KeyType>
std::string Engine<ItemType,KeyType>::makeUpper(std::string str) {
	for (unsigned int i = 0; i < str.length(); i++) {
		str[i] = toupper(str[i]);
	}

	return str;
}

template<typename ItemType, typename KeyType>
void Engine<ItemType,KeyType>::createTree(std::string filename) {
    ifstream file;
    file.open(filename);

    std::string line;

    while (std::getline(file,line)) {
        if (line == "") continue;

        std::string word = line.substr(0,  line.find(' '));
        std::string defn = line.substr(line.find(' ') + 1);

        // convert to all uppercase for easier handling
        for (unsigned int i = 0; i < word.length(); i++) {
            word[i] = toupper(word[i]);
        }

        Dictionary entry(word,defn);
        bst.add(entry);
    }

    file.close();
}

template<typename ItemType, typename KeyType>
void Engine<ItemType,KeyType>::printMenu() {
	std::cout << "\nInput a selection\n";
	std::cout << "1) Search the dictionary\n";
	std::cout << "2) Copy the original dictionary\n";
	std::cout << "3) Add an entry to the dictionary (can only be done to a copy)\n";
	std::cout << "4) Remove an entry from the dictionary\n";
	std::cout << "5) Save the dictionary to file\n";
	std::cout << "6) Run tests\n";
	std::cout << "7) Exit\n";
	std::cout << "\nYour choice: ";
}

template<typename ItemType, typename KeyType>
void Engine<ItemType,KeyType>::saveMenu(int order, int tree, ofstream& outFile) {
    if (tree == 1) {
        switch (order) {
            case 1: {
                bst.save(outFile, IN_ORDER);
                break;
            }
            case 2: {
                bst.save(outFile, PRE_ORDER);
                break;
            }
            case 3: {
                bst.save(outFile, POST_ORDER);
                break;
            }
        }
    } else if (tree == 2) {
        if (isTreeCopied == false) {
            std::cout << "\nERROR: Please make a copy of the tree first!\n\n";
            return;
        }

        switch (order) {
            case 1: {
                copyBst->save(outFile, IN_ORDER);
                break;
            }
            case 2: {
                copyBst->save(outFile, PRE_ORDER);
                break;
            }
            case 3: {
                copyBst->save(outFile, POST_ORDER);
                break;
            }
        }
    } else {
        std::cout << "\nInvalid input!\n";
    }

    std::cout << "\nSaved dictionary to file\n\n";
}

/////////////////////////////////////////////////
//      Tests Functions Section
/////////////////////////////////////////////////
template<typename ItemType, typename KeyType>
void Engine<ItemType,KeyType>::testAdds(BinarySearchTree<Dictionary, std::string> dictionary) {
    BinarySearchTree<Dictionary, std::string>* copy = new BinarySearchTree<ItemType, KeyType>(dictionary);

    std::string newWord;
    std::string newDefn;

    std::cout << "\nEnter a word you would like to add to the dictionary: ";
    std::cin >> newWord;
    cin.ignore();

    std::cout << "Enter the definition: ";
    std::getline(cin, newDefn);

    Dictionary newEntry(newWord, newDefn);

    if (copy->add(newEntry)) {
        std::cout << "\n" << makeUpper(newWord) << " addded to the dictionary\n\n";
    }

    copy->inorderTraverse(visit);

    delete copy;
}

template<typename ItemType, typename KeyType>
void Engine<ItemType,KeyType>::testRemoves(BinarySearchTree<Dictionary, std::string> dictionary) {
    BinarySearchTree<Dictionary, std::string>* copy = new BinarySearchTree<ItemType, KeyType>(dictionary);

    std::string wordToDelete;
    std::cout << "\nEnter a word you would like to remove from the dictionary: ";
    std::cin >> wordToDelete;

    try {
        copy->removeEntry(makeUpper(wordToDelete));
        std::cout << "\n" << makeUpper(wordToDelete) << " removed from the dictionary\n\n";
        copy->inorderTraverse(visit);
    } catch (NotFoundException e) {
        std::cerr << e.what();
    }

    delete copy;
}

template<typename ItemType, typename KeyType>
void Engine<ItemType,KeyType>::testWriteToFile(BinarySearchTree<Dictionary, std::string> dictionary) {
    std::string outputFilename;
    std::cout << "\nEnter the output file name: " ;
    std::cin >> outputFilename;

    ofstream outFile;
    outFile.open(outputFilename);

    dictionary.save(outFile, IN_ORDER);

    outFile.close();
}

/////////////////////////////////////////////////
//      Run engine Section
/////////////////////////////////////////////////
template<typename ItemType, typename KeyType>
void Engine<ItemType,KeyType>::run() {
    int choice;
	do {
		printMenu();
		std::cin >> choice;

		switch (choice) {
			case 1:	{
				std::string word;

				std::cout << "Enter a word to search: ";
				std::cin >> word;

				// convert to all uppercase for easier handling
				word = makeUpper(word);

				// search for the word
				try {
                    if (bst.contains(word))
                        std::cout << bst.getEntry(word);
                    else
                        std::cout << "\n*** Word not found ***\n\n";
				} catch (NotFoundException e) {
					std::cerr << e.what();
				}

				break;
			}
			case 2:	{
                copyBst = new BinarySearchTree<ItemType, KeyType>(bst);
                std::cout << "\nOriginal tree copied.\n\n";
                isTreeCopied = true;
				break;
			}
			case 3:	{
                if (isTreeCopied == false) {
                    std::cout << "\nPlease make a copy of the tree first!\n\n";
                    break;
                }

				std::string newWord;
                std::string newDefn;

				std::cout << "\nEnter a word you would like to add to the dictionary: ";
				std::cin >> newWord;
                cin.ignore();

                std::cout << "Enter the definition: ";
                std::getline(cin, newDefn);

                Dictionary newEntry(newWord, newDefn);

                if (copyBst->add(newEntry))
                    std::cout << "\n" << makeUpper(newWord) << " addded to the dictionary\n\n";

				break;
			}
            case 4: {
                if (isTreeCopied == false) {
                    std::cout << "\nPlease make a copy of the tree first!\n\n";
                    break;
                }

                std::string wordToDelete;
                std::cout << "\nEnter a word you would like to remove from the dictionary: ";
				std::cin >> wordToDelete;

                copyBst->removeEntry(wordToDelete);

                std::cout << "\n" << makeUpper(wordToDelete) << " removed from the dictionary\n\n";

                copyBst->inorderTraverse(visit);

                break;
            }
            case 5: {
                std::string outputFilename;
                std::cout << "\nEnter the output file name: " ;
                std::cin >> outputFilename;

                ofstream outFile;
                outFile.open(outputFilename);

                int whichTree;
                std::cout << "\nWhich tree would you like to save?\n";
                std::cout << "1) Original\n";
                std::cout << "2) Copy\n";
                std::cout << "\nYour choice: ";
                std::cin >> whichTree;

                int whichOrder;
                std::cout << "\nIn which order of traversal would you like to save?\n";
                std::cout << "1) Inorder\n";
                std::cout << "2) Preorder\n";
                std::cout << "3) Postorder\n";
                std::cout << "\nYour choice: ";
                std::cin >> whichOrder;

                saveMenu(whichOrder, whichTree, outFile);

                outFile.close();
                break;
            }
            case 6: {
                std::cout << "\nRunning test mode-\n\n";

                int testChoice;
                std::cout << "Choose a test:\n";
                std::cout << "1) Add an entry\n";
                std::cout << "2) Remove an entry\n";
                std::cout << "3) Write to file\n";
                std::cout << "\nYour choice: ";
                std::cin >> testChoice;

                if (testChoice == 1) {
                    testAdds(bst);
                } else if (testChoice == 2) {
                    testRemoves(bst);
                } else {
                    testWriteToFile(bst);
                }

                break;
            }

		} // end switch statement

	} while (choice != 7);

    std::cout << "\nExiting...\n\n";

}
