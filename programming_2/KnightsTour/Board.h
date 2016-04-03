/**
*	@file : Board.h
*	@author : Sharynne Azhar
*	@date : 03-07-2016
*	Purpose: Header file for the Board class used to initialize a board size
*/

#ifndef BOARD_H
#define BOARD_H

#include <iostream>
#include <iomanip>

#include "BoardCreationException.h"

class Board
{
    private:
        int** board;
        int boardRows;
        int boardCols;
        int knightStartingRow;
        int knightStartingCol;

    public:
        /*
        *   @param row/cols - board dimensions
        *   @param startRow/startCol - knights's starting position
        *   @post a Board object constructor
        */
        Board(int rows, int cols, int startRow, int startCol);

        /*
        *   @pre the board is created with valid dimensions
        *   @returns the number of rows on the board
        */
        int getBoardRows() const;

        /*
        *   @pre the board is created with valid dimensions
        *   @returns the number of columns on the board
        */
        int getBoardCols() const;

        /*
        *   @pre the board is created with a valid starting position for the knight
        *   @returns the row the knight is starting from
        */
        int getKnightStartRow() const;

        /*
        *   @pre the board is created with a valid starting position for the knight
        *   @returns the column the knight is starting from
        */
        int getKnightStartCol() const;

        /*
        *   @pre the board is created with valid dimensions
        *   @returns returns a double pointer aka the board
        */
        int** getBoard() const;

        /*
        *   @pre valid board dimension and starting position provided
        *   @post an instance of a Board object created = 2D array
        *   @throws BoardCreationException when invalid dimensions or starting position given
        */
        void createBoard() throw (BoardCreationException);

        /*
        *   @pre the board is created with valid dimensions
        *   @post prints the board to the console
        */
        void printBoard();
};

#endif
