/**
*	@file : main.cpp
*	@author : Sharynne Azhar
*	@date : 2015.11.12
*	@brief: Implementation file for Maze driver
**/

#include <iostream>

#include "Test.h"
#include "MazeReader.h"
#include "MazeWalker.h"
#include "MazeCreationException.h"

int main(int argc, char** argv)
{
    Test myTest;
    MazeReader* maze = nullptr;
    MazeWalker* walker = nullptr;

    if (argc != 3 && argc != 2)
    {
        std::cout << "Error. Invalid number of arguments.\n";
        return 0;
    }

    std::string searchType = argv[1];
    if (argc == 2 && searchType.compare("-test") == 0)
    {
        myTest.runTests();
        return 0;
    }

    std::string mazeFile = argv[2];
    if (!(searchType.compare("-dfs") == 0 || searchType.compare("-bfs") == 0))
    {
        std::cout << "Error. Invalid search type.\n";
        return 0;
    }

    try
    {
        maze = new MazeReader(mazeFile);
    }
    catch(MazeCreationException& e)
    {
        std::cout << e.what();
        return 0;
    }

    if (searchType.compare("-dfs") == 0)
    {
        walker = new MazeWalker(maze->getMaze(), maze->getStartRow(), maze->getStartCol(), maze->getRows(), maze->getCols(), Search::DFS);
    }
    else
    {
        walker = new MazeWalker(maze->getMaze(), maze->getStartRow(), maze->getStartCol(), maze->getRows(), maze->getCols(), Search::BFS);
    }

    std::cout << "\n================================================\n";
    std::cout << "\nStarting Position: "<<maze->getStartRow() << ", " << maze->getStartCol() << "\n";

	std::cout << "\nSize: " << maze->getRows() << ", " << maze->getCols() << "\n";
    std::cout << "\n================================================\n\n";


    // walk the maze
    bool escape = walker->walkMaze();

    // keep track of visited points
    const int* const* visited = walker->getVisited();

    // print the maze
    for (int i = 0; i < maze->getRows(); i++)
    {
        std::cout << "  ";
        for (int j = 0; j < maze->getCols(); j++)
        {
            std::cout << visited[i][j] << "\t";
        }
        std::cout << "\n\n";
    }

    std::cout << "\n================================================\n";
    if (escape)
    {
        std::cout << "\nWe escaped!\n";
    }
    else
    {
        std::cout << "\nNo way out!\n";
    }
    std::cout << "\n================================================\n";

    delete maze;
    maze = nullptr;

    delete walker;
    walker = nullptr;

    return 0;
}
