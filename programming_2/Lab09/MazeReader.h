/**
*	@file : MazeReader.h
*	@author : Sharynne Azhar
*	@date : 2015.11.12
*	@brief: Header file for MazeReader class
**/

#ifndef MAZEREADER_H
#define MAZEREADER_H

#include <iostream>
#include <fstream>

#include "MazeCreationException.h"

class MazeReader
{
private:
    char** m_maze; // 2D array to hold information from file
    int m_cols;
    int m_rows;
    int m_startCol;
    int m_startRow;

    std::ifstream mazeFile; // file object used to read in maze

public:
    /**
    *	@post A MazeReader is created. A 2D char array is allocated with the maze information.
    *	@throws MazeCreationExecption
    */
    MazeReader(std::string file) throw (MazeCreationException);

    /**
    *	@post The maze is deallocated.
    */
    ~MazeReader();

    /**
    *	@pre the file was formatting and read in correctly
    *	@return Returns pointer to the maze
    */
    const char* const* getMaze() const;

    /**
    *	@pre the file was formatted and read in correctly
    *	@returns the number of columns listed in the file
    */
    int getCols() const;

    /**
    *	@pre the file was formatted and read in correctly
    *	@returns the number of rows listed in the file
    */
    int getRows() const;

    /**
    *	@pre the file was formatted and read in correctly
    *	@returns the starting column
    */
    int getStartCol() const;

    /**
    *	@pre the file was formatted and read in correctly
    *	@returns the starting row
    */
    int getStartRow() const;

protected:
    /**
    *	@pre the file is properly formatted
    *	@post the characters representing the maze are stores
    */
    void readMaze()	throw (MazeCreationException);
};

#endif
