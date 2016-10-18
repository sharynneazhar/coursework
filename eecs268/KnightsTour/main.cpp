/**
*	@file : main.cpp
*	@author : Sharynne Azhar
*	@date : 03-07-2016
*	Purpose: main driver
*/

#include <iostream>

#include "Board.h"
#include "KnightsTour.h"

int main(int argc, char** argv)
{
    std::cout << "\n=================\n\n";
    std::cout << " KNIGHT'S TOUR    \n";
    std::cout << "\n=================\n\n";

    if (argc < 4)
    {
        std::cerr << "Invalid arguments: Missing parameters to create board\n\n";
        std::cerr << "Four parameters should be as follows:\n";
        std::cerr << "   m, n, initial-knight-row, initial-knight-col\n\n";
        return 0;
    }

    int rows = std::atoi(argv[1]);
    int cols = std::atoi(argv[2]);
    int startRow = std::atoi(argv[3]);
    int startCol = std::atoi(argv[4]);

    Board Board(rows, cols, startRow, startCol);
    KnightsTour KnightsTour;

    try
    {
        Board.createBoard();
        KnightsTour.run(Board);
    }
    catch (BoardCreationException& e)
    {
        std::cout << e.what() << "\n\n";
    }

    return 0;
}
