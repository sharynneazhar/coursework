/**
*	@file : Board.cpp
*	@author : Sharynne Azhar
*	@date : 03-07-2016
*	Purpose: Implementation file for the Board class
*/

#include "Board.h"

Board::Board(int rows, int cols, int startRow, int startCol) {
    boardRows = rows;
    boardCols = cols;
    knightStartingRow = startRow;
    knightStartingCol = startCol;
}

int Board::getBoardRows() const
{
    return boardRows;
}

int Board::getBoardCols() const
{
    return boardCols;
}

int Board::getKnightStartRow() const
{
    return knightStartingRow;
}

int Board::getKnightStartCol() const
{
    return knightStartingCol;
}

int** Board::getBoard() const
{
    return board;
}

void Board::createBoard() throw (BoardCreationException)
{
    if (boardRows < 1 || boardCols < 1)
    {
        throw BoardCreationException("Invalid maze dimensions.");
    }

    if (knightStartingRow < 0 || knightStartingRow >= boardRows || knightStartingCol < 0 || knightStartingCol >= boardCols)
    {
        throw BoardCreationException("Invalid starting position.");
    }

    if (boardRows >= 8 || boardCols >= 8)
    {
        throw BoardCreationException("This might take awhile to render... \nI'm killing this process");
    }

    // create the board (2D Array)
    board = new int*[boardRows];
    for (int i = 0; i < boardRows; i++)
    {
        board[i] = new int[boardCols];
    }

    // initialize the board with -1
    for (int i = 0; i < boardRows; i++)
    {
        for (int j = 0; j < boardCols; j++)
        {
            if (i == knightStartingRow && j == knightStartingCol)
                board[i][j] = 0;
            else
                board[i][j] = -1; // use -1 to fill the board since 0 is used to denote the starting step
        }
    }
}

void Board::printBoard()
{
    for (int i = 0; i < boardRows; i++)
    {
        for (int j = 0; j < boardCols; j++)
        {
            if (board[i][j] < 0)
                std::cout << "[" << std::setw(1) << "  ]";
            else
                std::cout << "[" << std::setw(2) << board[i][j] << "]";
        }
        std::cout << "\n";
    }
}
