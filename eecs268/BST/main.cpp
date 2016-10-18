/**
*	@file : main.cpp
*	@author : Sharynne Azhar
*	@date : 2015.11.08
*	@brief: Implementation for main driver program
*/

#include <iostream>
#include <climits>
#include <random>
#include <ctime>

#include "BinarySearchTree.h"
#include "Test.h"

void printMenu()
{
	std::cout<<"\n\nInput a selection\n";
	std::cout<<"1) Add more values to original tree\n";
	std::cout<<"2) Copy the original tree\n";
	std::cout<<"3) Delete the original tree (one time only)\n";
	std::cout<<"4) Print original tree\n";
	std::cout<<"5) Print copy\n";
	std::cout<<"6) Search original tree\n";
	std::cout<<"7) Search copy\n";
	std::cout<<"8) Exit\n";
	std::cout<<"9) Run tests\n";
	std::cout<<"Your choice: ";
}

int main(int argc, char** argv)
{
    if (!(argc == 2 || argc == 3))
    {
    	std::cout << "Invalid arguments\n";
    	return 0;
    }

    std::string argType = argv[1];
    Test myTest;
    int numNodes = 0;

    if (argType.compare("-nodes") == 0 && argc == 3)
    {
    	numNodes = atoi(argv[2]);
    }
    else if (argType.compare("-test") == 0)
    {
    	myTest.runTests();
    }
    else
    {
    	std::cout << "Invalid command line arguments.";
    	return 0;
    }

    if(numNodes < 0)
    {
    	std::cout<<"Cannot make a tree with negative number of nodes.\n ";
    	return 0;
    }

    // create tree
	BinarySearchTree<int>* original = new BinarySearchTree<int>();
	BinarySearchTree<int>* copy = nullptr;

    // generate some random numbers
	std::default_random_engine generator(time(nullptr));
	std::uniform_int_distribution<int> distribution(INT_MIN,INT_MAX);

    // fill tree
	std::cout << "Filling original tree with " << numNodes << " values.\n";

    int value = 0;
    for(int i = 0; i < numNodes; i++)
    {
    	value = distribution(generator);
    	if (original->add(value))
        {
    		std::cout << "Adding " << value << " to original tree.\n";
    	}
    }
    value = 0; // reset the value

    int choice = 0;
    bool flag = false;
    while(!flag)
    {
    	printMenu();
    	std::cin >> choice;
    	std::cout << "You chose: " << choice << "\n";

    	switch(choice)
        {
    		case 1:
            {
    			if (original == nullptr)
                {
    				std::cout << "Original tree does not exist. Cannot add.\n";
    			}
    			else
                {
    				std::cout << "Input a value to add to the original: ";
    				std::cin >> value;

                	if (original->add(value))
                    {
    					std::cout << "Adding " << value << " to the original tree.\n";
    				}
    				else
                    {
    					std::cout << "Could not add value to the original tree.\n";
    				}
    			}
    			break;
            }
    		case 2:
            {
    			copy = new BinarySearchTree<int>(*original);
    			std::cout << "Original tree copied.\n";
    		    break;
            }
    		case 3:
            {
    			delete original;
    			original = nullptr;
    			std::cout << "Original Tree deleted.\n";
    			break;
            }
    		case 4:
    		{
    			if (original == nullptr)
                {
    			    std::cout << "Original Tree no longer exists. Cannot print.\n";
    			}
    			else
                {
    				std::cout << "Print order options: \n";
    				std::cout << "\t0 - pre-order\n";
    				std::cout << "\t1 - in-order\n";
    				std::cout << "\t2 - post-order\n";

    				std::cout << "Your choice: ";
    				std::cin >> value;
    				std::cout << "You chose: " << value << "\n";

    				switch(value)
                    {
    					case 0:
    						original->printTree(PRE_ORDER);
    						break;
    					case 1:
    						original->printTree(IN_ORDER);
    					    break;
    					case 2:
    						original->printTree(POST_ORDER);
    						break;
    					default:
    						std::cout << "Not a valid option.\n";
    						break;
    				} // end switch value
    			}
    			break;
            }
    		case 5:
            {
            	if (copy == nullptr)
                {
    				std::cout<<"This tree doesn't exist.  Cannot print.\n";
    			}
    			else
                {
    				std::cout<<"Print order options: \n";
    				std::cout<<"\t0 - pre-order\n";
    				std::cout<<"\t1 - in-order\n";
    				std::cout<<"\t2 - post-order\n";

    				std::cout<<"Your choice: ";
    				std::cin>>value;
    				std::cout<<"You chose: "<<value<<"\n";

    				switch(value)
                    {
    					case 0:
    						copy->printTree(PRE_ORDER);
    						break;
    					case 1:
    						copy->printTree(IN_ORDER);
    						break;
    					case 2:
    						copy->printTree(POST_ORDER);
    						break;
    					default:
    						std::cout<<"Not a valid option.\n";
    						break;
    				} // end switch
    			}
    			break;
            }
    		case 6:
    		{
    			if (original == nullptr)
                {
    				std::cout<<"Tree does not exist.  Cannot print.\n";
    			}
    			else
                {
    				std::cout<<"Choose a value you wish to search for: \n";
    				std::cin>>value;

    				if(original->search(value))
                    {
    					std::cout<<value<<" is in the list.\n";
    				}
    				else
                    {
    					std::cout<<value<<" is not in the list.\n";
    				}
    			}
    			break;
            }
			case 7:
    		{
    			if(copy == nullptr)
                {
    				std::cout<<"This tree does not exist.  Cannot print.\n";
    			}
    			else
                {
    				std::cout<<"Choose a value you wish to search for: \n";
    				std::cin>>value;

    				if(copy->search(value))
                    {
    					std::cout<<value<<" is in the list.\n";
    				}
    				else
                    {
    					std::cout<<value<<" is not in the list.\n";
    				}
    			}
    			break;
            }
    		case 8:
            {
            	flag = true;
    			break;
            }
    		case 9:
            {
                myTest.runTests();
    			break;
            }
    		default:
            {
    			std::cout<<"Invalid choice.\n";
    			break;
            }
    	} // end menu switch
    } // end while

    std::cout<<"Exiting...";

	if (original != nullptr)
    {
		delete original;
		original = nullptr;
	}

	delete copy;
	copy = nullptr;

	return 0;
}
