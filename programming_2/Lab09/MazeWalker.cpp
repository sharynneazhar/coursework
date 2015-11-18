/**
*	@file : MazeWalker.cpp
*	@author : Sharynne Azhar
*	@date : 2015.11.12
*	@brief: Implementation file for MazeWalker class
**/

#include "MazeWalker.h"

// creates an object to traverse the maze
MazeWalker::MazeWalker(const char* const* mazePtr, int startRow, int startCol, int rows, int cols, Search searchChoice) : m_curPos(Position(startRow, startCol)), m_startPos(Position(startRow, startCol))
{
    m_searchType = searchChoice;
    m_maze = mazePtr;
    m_rows = rows;
    m_cols = cols;
    m_curStep = 1;

    // m_visited is used to traverse the maze and mark visited locations
    m_visited = new int*[m_rows];

    for (int i = 0; i < m_rows; i++)
    {
        m_visited[i] = new int[m_cols];
    }

    // read in the maze
    for (int i = 0; i < m_rows; i++)
    {
        for (int j = 0; j < m_cols; j++)
        {
            m_visited[i][j] = 0;
        }
    }

    m_visited[startRow][startCol] = m_curStep;
    m_curStep++;
}

MazeWalker::~MazeWalker()
{
    for (int i = 0; i < m_rows; i++)
    {
        delete [] m_visited[i];
        m_visited[i] = nullptr;
    }

    delete [] m_visited;
    m_visited = nullptr;
}

bool MazeWalker::walkMaze()
{
    if (m_searchType == Search::DFS)
    {
        // load starting position
        m_moveStack.push(m_startPos);

        // traverse the maze!
        while (!(m_moveStack.empty() || isGoalReached()))
        {
            storeValidMoves();
            move(m_moveStack.top());
            m_moveStack.pop();
        }
    }
    else // BFS
    {
        m_moveQueue.push(m_startPos);

        while (!(m_moveQueue.empty() || isGoalReached()))
        {
            storeValidMoves();
            m_moveQueue.pop();

            if (!m_moveQueue.empty())
            {
                move(m_moveQueue.front());
            }
        }
    }

    return isGoalReached();
}

const int* const* MazeWalker::getVisited() const
{
    return m_visited;
}

int MazeWalker::getVisitedRows() const
{
    return m_rows;
}

int MazeWalker::getVisitedCols() const
{
    return m_cols;
}

const char* const* MazeWalker::getMaze() const
{
    return m_maze;
}

void MazeWalker::storeValidMoves()
{
    // check if move is possible
    bool up = m_curPos.getRow() - 1 >= 0;
    bool right = m_curPos.getCol() + 1 < m_cols;
    bool down = m_curPos.getRow() + 1 < m_rows;
    bool left = m_curPos.getCol() - 1 >= 0;

    // make life easy aka make code easy to read
    int moveUp = m_curPos.getRow() - 1;
	int moveDown = m_curPos.getRow() + 1;
	int moveLeft = m_curPos.getCol() - 1;
	int moveRight = m_curPos.getCol() + 1;

    int currRow = m_curPos.getRow();
    int currCol = m_curPos.getCol();

    if (m_searchType == Search::DFS)
    {
        // must check all 4 moves
        if (up)
        {
            if (m_maze[moveUp][currCol] != 'W' && m_visited[moveUp][currCol] == 0)
            {
                m_moveStack.push(Position(moveUp, currCol));
            }
        }

        if (right)
        {
            if (m_maze[currRow][moveRight] != 'W' && m_visited[currRow][moveRight] == 0)
            {
                m_moveStack.push(Position(currRow, moveRight));
            }
        }

        if (down)
        {
            if (m_maze[moveDown][currCol] != 'W' && m_visited[moveDown][currCol] == 0)
            {
                m_moveStack.push(Position(moveDown, currCol));
            }
        }

        if (left)
        {
            if (m_maze[currRow][moveLeft] != 'W' && m_visited[currRow][moveLeft] == 0)
            {
                m_moveStack.push(Position(currRow, moveLeft));
            }
        }
    }
    else //BFS
    {
        if (up)
        {
            if (m_maze[moveUp][currCol] != 'W' && m_visited[moveUp][currCol] == 0)
            {
                m_moveQueue.push(Position(moveUp, currCol));
            }
        }

        if (right)
        {
            if (m_maze[currRow][moveRight] != 'W' && m_visited[currRow][moveRight] == 0)
            {
                m_moveQueue.push(Position(currRow, moveRight));
            }
        }

        if (down)
        {
            if (m_maze[moveDown][currCol] != 'W' && m_visited[moveDown][currCol] == 0)
            {
                m_moveQueue.push(Position(moveDown, currCol));
            }
        }

        if (left)
        {
            if (m_maze[currRow][moveLeft] != 'W' && m_visited[currRow][moveLeft] == 0)
            {
                m_moveQueue.push(Position(currRow, moveLeft));
            }
        }
    }
}

void MazeWalker::move(Position& p)
{
    m_curPos = p;

    if (m_visited[m_curPos.getRow()][m_curPos.getCol()] == 0)
    {
        m_visited[m_curPos.getRow()][m_curPos.getCol()] = m_curStep;
        m_curStep++;
    }
}

bool MazeWalker::isGoalReached() const
{
    if (m_maze[m_curPos.getRow()][m_curPos.getCol()] == 'E')
    {
        return true;
    }

    return false;
}
