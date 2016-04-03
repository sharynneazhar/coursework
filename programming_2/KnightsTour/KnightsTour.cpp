/**
*	@file : KnightsTour.cpp
*	@author : Sharynne Azhar
*	@date : 03-07-2016
*	Purpose: Implementation file for the KnightsTour class used to solve the problem
*/

#include "KnightsTour.h"

bool KnightsTour::isValidMove(int rowPos, int colPos, int** board)
{
    // check if the knight's next move will be within the board size
    // check if the knight's next move will be on an unoccupied space
    if ((rowPos >= 0 && rowPos < maxRows) && (colPos >= 0 && colPos < maxCols) && (board[rowPos][colPos] == -1))
        return true;

    return false;
}

/*
*    Algorithm
*        If all spaces are visited then return true
*        ELse,
*            Go through the list of all possible moves (8 total)
*            Pick one move and check if it's valid
*                If it is then mark it as visited and move on to the next step
*                Keep going until all spaces are filled
*            If a move doesn't work then backtrack to the last valid move and try a different next move
*        If none of the moves work and the board isn't complete then return false
*/
bool KnightsTour::search(int currRow, int currCol, int currStep, int** board)
{
    int nextRowPos;
    int nextColPos;

    // return true when all spaces have been visited
    if (currStep == maxSteps)
        return true;

    // go through all 8 possible moves
    for (int i = 0; i < 8; i++)
    {
        // pick a move from the list of moves
        nextRowPos = currCol + movesX[i];
        nextColPos = currRow + movesY[i];

        if (isValidMove(nextRowPos, nextColPos, board))
        {
            // move is valid so mark as visited and move to next step
            board[nextRowPos][nextColPos] = currStep + 1;

            // keep going and return true once all spaces are visited
            if (search(nextRowPos, nextColPos, currStep + 1, board))
                return true;
            else
                board[nextRowPos][nextColPos] = -1; // backtrack and move on to next move if invalid
        }
    }

    // If nothing works, then return false
    return false;
}

void KnightsTour::run(Board board)
{
    maxRows = board.getBoardRows();;
    maxCols = board.getBoardCols();
    maxSteps = (maxRows * maxCols) - 1;

    int currStep = 0;

    int knightXStart = board.getKnightStartCol();
    int knightYStart = board.getKnightStartRow();

    tourBoard = board.getBoard();
    if (search(knightYStart, knightXStart, currStep, tourBoard))
        std::cout << "Success!\n\n";
    else
        std::cout << "Failed!\n\n";

    board.printBoard();
}
