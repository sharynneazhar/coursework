/**
*	@file Test.h
* 	@author Joseph St. Amand, John Gibbons
*	@date 2015.10.27
*
*/
#ifndef TEST_H
#define TEST_H

#include <iostream>
#include <vector>
#include <string>
#include <climits>
#include <algorithm> 

#include "MazeReader.h"
#include "MazeWalker.h"
#include "ArrayHelper.h"

class Test
{
public:
        Test();
        void runTests();

	bool test_mazeReader01(); //MazeReader throws MazeCreationException if file doesn't exist
	bool test_mazeReader02(); //MazeReader throws MazeCreationException if rows are less than 1
	bool test_mazeReader03(); //MazeReader throws MazeCreationException if cols are less than 1
	bool test_mazeReader04(); //MazeReader throws MazeCreationException if start position is outside ranges described by rows and cols in file
	bool test_mazeReader05(); //MazeReader getRows and getCols return correct values on valid maze
	bool test_mazeReader06(); //MazeReader getStartRow and getStartCol return correct values on valid maze
	bool test_mazeReader07(); //MazeReader getMaze returns a 2D char array with all maze characters in it

	bool test_mazeWalker01(); //MazeWalker finds exit in maze.txt in correct order using dfs
	bool test_mazeWalker02(); //MazeWalker finds exit in maze.txt in correct order using bfs
	bool test_mazeWalker03(); //MazeWalker visits all reachable positions in noExit.txt in correct order using dfs
	bool test_mazeWalker04(); //MazeWalker visits all reachable positions in noExit.txt in correct order using bfs

private:
        int m_testNum;
        const int TEST_SIZE; //stress test size
        const int MAX_SCORE; //maximum points available from tests


        /**
        *  @pre None.
        *  @post Uses std::cerr to print the Pass/Fail message
        *  @return None.
        *
        */
        void printPassFail(bool isPassed) const;

        /**
        *  @pre None.
        *  @post Uses std::cerr to print the test number and description
        *  @return None.
        *
        */
        void printTestMessage(int testNum, std::string testDescription) const;

        /**
        *  @pre None.
        *  @post None.
        *  @return True if the file exists, false otherwise.
        *
        */
        bool isFileAccessible(std::string fileName) const;

        /**
        *  @pre None.
        *  @post The vector is printed.
        *
        */
	void printVector(const std::vector<int>& vec);

	/**
	*  @pre none
	*  @return true if a MazeCreationException was thrown when opening the test file
	*/
	bool testBadFile(std::string testFileName, std::string testMessage);
	
	/**
	*  @pre testFileName is valid maze file and correctArray is the correct traversal order (of given size).	
	*  @post Creates a MazeWalker, traverses the given maze, and compares the Walker visited array to the correct array.
	*  @return true if the MazeWalker's getVisited maze is equal to the correctArray
	*/
	bool testWalker(std::string testFileName, const int* const* correctArray, int correctRows, int correctCols, Search searchChoice);
};

#endif
