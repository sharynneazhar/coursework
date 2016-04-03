/**
*	@file : KnightsTour.h
*	@author : Sharynne Azhar
*	@date : 03-07-2016
*	Purpose: Header file for the KnightsTour class used to solve the problem
*/

#ifndef KNIGHTS_TOUR_H
#define KNIGHTS_TOUR_H

#include <iostream>
#include "Board.h"

/*
*   Possible combination of moves by a knight in x and y coordinates
*   For example, (2, -1) is right 2 and down 1
*/
const int movesX[8] = {2, 1, -2, -1, 2, 1, -2, -1};
const int movesY[8] = {-1, -2, -1, -2, 1, 2, 1, 2};

class KnightsTour
{
    private:
        int maxSteps;
        int maxRows;
        int maxCols;
        int** tourBoard;

    public:
        /*
        *   @param rowPos/colPos - row/column position of the knight's next move
        *   @param board - pointer to the 2D array
        *   @pre the board pointer is pointing to an instance of a valid 2D array
        *   @returns true if the move is within the board and unvisited, false otherwise
        */
        bool isValidMove(int rowPos, int colPos, int** board);

        /*
        *   @param currRow/currCol - current row/column position of the knightStartingRow
        *   @param currStep - current step the knight is on
        *   @param board - pointer to the 2D array
        *   @pre the board pointer is pointing to an instance of a valid 2D array
        *   @post implements the recursive backtracking of the board
        *   @returns true if all spaces on the board are visited, false otherwise
        */
        bool search(int currRow, int currCol, int currStep, int** board);

        /*
        *   @param board object
        *   @pre an instance of the board object was created
        *   @post determines the max number of steps, run the search, and prints the result
        */
        void run(Board board);
};

#endif
