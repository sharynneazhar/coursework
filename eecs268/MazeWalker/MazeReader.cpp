/**
*	@file : MazeReader.cpp
*	@author : Sharynne Azhar
*	@date : 2015.11.12
*	@brief: Implementation file for MazeReader class
*/

#include "MazeReader.h"

MazeReader::MazeReader(std::string file) throw (MazeCreationException)
{
    mazeFile.open(file);

    if (mazeFile.fail())
    {
        throw MazeCreationException("Failed to open file.");
    }

    // read the file for maze information
    mazeFile >> m_rows >> m_cols >> m_startRow >> m_startCol;

    // check for a valid maze
    if (m_rows < 1 || m_cols < 1)
    {
        throw MazeCreationException("Invalid maze dimensions.");
    }

    if (m_startRow < 0 || m_startRow > m_rows || m_startCol < 0 || m_startCol > m_cols)
    {
        throw MazeCreationException("Invalid starting position.");
    }

    // create a 2D array for the maze
    m_maze = new char*[m_rows];
    for (int i = 0; i < m_rows; i++)
    {
        m_maze[i] = new char[m_cols];
    }

    // store maze into 2D array
    try
    {
        readMaze();
    }
    catch(MazeCreationException& e)
    {
        std::cout << e.what();
    }
}

MazeReader::~MazeReader()
{
    for (int i = 0; i < m_rows; i++)
    {
        delete [] m_maze[i];
        m_maze[i] = nullptr;
    }

    delete [] m_maze;
    m_maze = nullptr;
}

const char* const* MazeReader::getMaze() const
{
    return m_maze;
}

int MazeReader::getCols() const
{
    return m_cols;
}

int MazeReader::getRows() const
{
    return m_rows;
}

int MazeReader::getStartCol() const
{
    return m_startCol;
}

int MazeReader::getStartRow() const
{
    return m_startRow;
}

void MazeReader::readMaze()	throw (MazeCreationException)
{
    for (int i = 0; i < m_rows; i++)
    {
        for (int j = 0; j < m_cols; j++)
        {
            mazeFile >> m_maze[i][j];
        }
    }

    mazeFile.close();
}
