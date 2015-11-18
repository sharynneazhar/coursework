#include "Test.h"

void Test::runTests()
{

        int score = 0;

	//Run tests, track total points
        std::cerr << "\n\n=========================\n";
        std::cerr << "   RUNNING TEST SUITE    \n";
        std::cerr << "=========================\n\n";

        std::cerr << "\n\n=========================\n";
        std::cerr << "   RUNNING MAZE READER TESTS    \n";
        std::cerr << "=========================\n\n";

	score += test_mazeReader01() ? 3 : 0; 	
	score += test_mazeReader02() ? 3 : 0; 
	score += test_mazeReader03() ? 3 : 0; 	
	score += test_mazeReader04() ? 3 : 0; 
	score += test_mazeReader05() ? 3 : 0; 
	score += test_mazeReader06() ? 3 : 0; 
	score += test_mazeReader07() ? 10 : 0; 


        std::cerr << "\n\n=========================\n";
        std::cerr << "   RUNNING MAZE WALKER TESTS    \n";
        std::cerr << "=========================\n\n";

	score += test_mazeWalker01() ? 13 : 0;
	score += test_mazeWalker02() ? 13 : 0;
	score += test_mazeWalker03() ? 13 : 0;
	score += test_mazeWalker04() ? 13 : 0;

        std::cerr << "\n\n=========================\n";
        std::cerr << "Score: " << score << " / " << MAX_SCORE << "\n";
        std::cerr << "=========================\n" << std::endl;


}


Test::Test() : TEST_SIZE(10000), MAX_SCORE(80)
{
        m_testNum = 0;
}


void Test::printPassFail(bool isPassed) const
{
        if(isPassed)
                std::cerr << "PASSED" << std::endl;
        else
                std::cerr << "FAILED" << std::endl;
}

void Test::printTestMessage(int testNum, std::string testDescription) const
{
        std::cerr << "Test " << testNum << ": " << testDescription << ": ";
}

void Test::printVector(const std::vector<int>& vec)
{
	std::cerr << "{";
	for(std::size_t i=0; i<vec.size(); i++)
	{
		std::cerr << vec[i];
		if(i != vec.size()-1)
		{
			std::cerr << ",";
		}
	}
	std::cerr << "}";	
}

bool Test::isFileAccessible(std::string fileName) const
{
	std::ifstream file(fileName);
	bool isFileGood = file.good();
	file.close();
	return( isFileGood );
}

bool Test::testBadFile(std::string testFileName, std::string testMessage)
{
	bool isPassed = false;
	m_testNum++;

	printTestMessage(m_testNum, testMessage);

	if( isFileAccessible(testFileName) )
	{
		try
		{
			MazeReader mr(testFileName);
		}
		catch(MazeCreationException& mce)
		{
			isPassed = true;
		}
		catch(...)
		{
			std::cerr << "ERROR: Exception thrown was not MazeCreationException" << std::endl;
		}
	}
	else
	{
		std::cerr << "ERROR " << testFileName << " not accessible. Place in lab folder." << std::endl;		
	}

	printPassFail(isPassed);
	
	return(isPassed);
}



bool Test::test_mazeReader01()
{
	bool isPassed = false;
	

	m_testNum++;


	printTestMessage(m_testNum, "MazeReader throws MazeCreationException if file doesn't exist");

	try
	{
		MazeReader mr("");
	}
	catch(MazeCreationException& mce)
	{
		isPassed = true;
	}
	catch(...)
	{
		std::cerr << "ERROR: Exception thrown was not MazeCreationException" << std::endl;
	}

	printPassFail(isPassed);

	return(isPassed);
}

bool Test::test_mazeReader02()
{
	bool isPassed = false;
	std::string testFileName = "badRows.txt";

	isPassed = testBadFile(testFileName, "MazeReader throws MazeCreationException if rows are less than 1");	

	return(isPassed);
}


bool Test::test_mazeReader03()
{
	bool isPassed = false;
	std::string testFileName = "badCols.txt";

	isPassed = testBadFile(testFileName, "MazeReader throws MazeCreationException if cols are less than 1");	

	return(isPassed);
}

bool Test::test_mazeReader04()
{
	bool isPassed = false;
	std::string testFileName = "badStart.txt";

	isPassed = testBadFile(testFileName, "MazeReader throws MazeCreationException if start position is outside ranges described by rows and cols in file");	

	return(isPassed);
}

bool Test::test_mazeReader05()
{
	bool isPassed = false;
	std::string testFileName = "maze.txt";
	const int correctRows = 8;
	const int correctCols = 6;
	m_testNum++;
	
	printTestMessage(m_testNum, "MazeReader getRows and getCols return correct values on valid maze");	

	if(isFileAccessible(testFileName))
	{
		try
		{
			MazeReader mr(testFileName);
			if( (correctRows == mr.getRows()) && (correctCols == mr.getCols()) )
			{
				isPassed = true;
			}
			else
			{
				std::cerr << "ERROR: expected " << correctRows << " and " << correctCols 
					<< " got " << mr.getRows() << " and " << mr.getCols() << std::endl;
			}
		}
		catch(...)
		{
			std::cerr << "ERROR: exception thrown with valid file" << std::endl;
		}
	}
	else
	{
		std::cerr << "ERROR " << testFileName << " not accessible. Place in lab folder." << std::endl;		
	}	
	
	printPassFail(isPassed);
	
	return(isPassed);
}



bool Test::test_mazeReader06()
{
	bool isPassed = false;
	std::string testFileName = "maze.txt";
	const int correctRows = 5;
	const int correctCols = 4;
	m_testNum++;
	
	printTestMessage(m_testNum, "MazeReader getStartRow and getStartCol return correct values on valid maze");	

	if(isFileAccessible(testFileName))
	{
		try
		{
			MazeReader mr(testFileName);
			if( (correctRows == mr.getStartRow()) && (correctCols == mr.getStartCol()) )
			{
				isPassed = true;
			}
			else
			{
				std::cerr << "ERROR: expected " << correctRows << " and " << correctCols 
					<< " got " << mr.getStartRow() << " and " << mr.getStartCol() << std::endl;
			}
		}
		catch(...)
		{
			std::cerr << "ERROR: exception thrown with valid file" << std::endl;
		}
	}
	else
	{
		std::cerr << "ERROR " << testFileName << " not accessible. Place in lab folder." << std::endl;		
	}	
	
	printPassFail(isPassed);
	
	return(isPassed);
}



bool Test::test_mazeReader07()
{
	bool isPassed = false;
	std::string testFileName = "maze.txt";
	const int correctRows = 8;
	const int correctCols = 6;
	char** correctArray = new char*[correctRows];

	correctArray[0] = new char[correctCols]{'E','W','W','W','W','W'};
	correctArray[1] = new char[correctCols]{'P','P','P','P','P','P'};
	correctArray[2] = new char[correctCols]{'W','W','P','W','W','P'};
	correctArray[3] = new char[correctCols]{'P','P','P','W','W','P'};
	correctArray[4] = new char[correctCols]{'W','P','W','W','W','W'};
	correctArray[5] = new char[correctCols]{'W','P','P','P','S','W'};
	correctArray[6] = new char[correctCols]{'W','W','W','W','W','W'};
	correctArray[7] = new char[correctCols]{'W','W','W','W','W','W'};
	
	m_testNum++;	
	printTestMessage(m_testNum, "MazeReader getMaze returns a 2D char array with all maze characters in it");	

	try
	{
		MazeReader mr(testFileName);
		isPassed = ArrayHelper<char>::areArraysEqual(correctArray, correctRows, correctCols, mr.getMaze(), mr.getRows(), mr.getCols());

		if(!isPassed)
		{
			std::cerr << "ERROR: expected the following array: " << std::endl;
			ArrayHelper<char>::print2DArray(correctArray, correctRows, correctCols, " ");	
			std::cerr << "MazeReader produced the following array (formatted print by ArrayHelper): " << std::endl;
			ArrayHelper<char>::print2DArray(mr.getMaze(), mr.getRows(), mr.getCols(), " ");
		}
	}
	catch(...)
	{
		std::cerr << "ERROR: Unexpected exception thrown" << std::endl;
	}
	
	printPassFail(isPassed);

	for(int i=0; i<correctRows; i++)
	{
		delete[] correctArray[i];
	}

	delete[] correctArray;


	return(isPassed);
}


bool Test::testWalker(std::string testFileName, const int* const* correctArray, int correctRows, int correctCols, Search searchChoice)
{
	bool isPassed = false;

	try
	{
		MazeReader mr(testFileName);
		MazeWalker walker(mr.getMaze(), mr.getStartRow(), mr.getStartCol(), 
						 mr.getRows(), mr.getCols(), searchChoice);

		walker.walkMaze();

		isPassed = ArrayHelper<int>::areArraysEqual(correctArray, correctRows, correctCols, walker.getVisited(), mr.getRows(), mr.getCols());

		if(!isPassed)
		{
			std::cerr << "ERROR: expected the following array: " << std::endl;
			ArrayHelper<int>::print2DArray(correctArray, correctRows, correctCols, "\t");	
			std::cerr << "MazeWalker produced the following array (formatted print by ArrayHelper): " << std::endl;
			ArrayHelper<int>::print2DArray(walker.getVisited(), walker.getVisitedRows(), walker.getVisitedCols(), "\t");
		}
	}
	catch(...)
	{
		std::cerr << "ERROR: Unexpected exception thrown" << std::endl;
		isPassed = false;
	}
	
	return(isPassed);
}

bool Test::test_mazeWalker01()
{
	bool isPassed = false;
	std::string testFileName = "maze.txt";
	const int correctRows = 8;
	const int correctCols = 6;
	int** correctArray = new int*[correctRows];
	Search searchChoice = Search::DFS;	

	correctArray[0] = new int[correctCols]{13,	  0,	  0,	  0,	  0,	  0};
	correctArray[1] = new int[correctCols]{12,	  11,	  10,	  0,	  0,	  0};
	correctArray[2] = new int[correctCols]{0,	  0,	  9,	  0,	  0,	  0};
	correctArray[3] = new int[correctCols]{7,	  6,	  8,	  0,	  0,	  0};
	correctArray[4] = new int[correctCols]{0,	  5,	  0,	  0,	  0,	  0};
	correctArray[5] = new int[correctCols]{0,	  4,	  3,	  2,	  1,	  0};
	correctArray[6] = new int[correctCols]{0,	  0,	  0,	  0,	  0,	  0};
	correctArray[7] = new int[correctCols]{0,	  0,	  0,	  0,	  0,	  0};

	m_testNum++;	
	printTestMessage(m_testNum, "MazeWalker finds exit in maze.txt in correct order using dfs");

	isPassed = testWalker(testFileName, correctArray, correctRows, correctCols, searchChoice);	

	printPassFail(isPassed);

	for(int i=0; i<correctRows; i++)
	{
		delete[] correctArray[i];
	}

	delete[] correctArray;


	return(isPassed);
}

bool Test::test_mazeWalker02()
{
	bool isPassed = false;
	std::string testFileName = "maze.txt";
	const int correctRows = 8;
	const int correctCols = 6;
	int** correctArray = new int*[correctRows];
	Search searchChoice = Search::BFS;	

	correctArray[0] = new int[correctCols]{16,	  0,	  0,	  0,	  0,	  0};
	correctArray[1] = new int[correctCols]{14,	  12,	  10,	  11,	  13,	  15};
	correctArray[2] = new int[correctCols]{0,	  0,	  9,	  0,	  0,	  0};
	correctArray[3] = new int[correctCols]{8,	  6,	  7,	  0,	  0,	  0};
	correctArray[4] = new int[correctCols]{0,	  5,	  0,	  0,	  0,	  0};
	correctArray[5] = new int[correctCols]{0,	  4,	  3,	  2,	  1,	  0};
	correctArray[6] = new int[correctCols]{0,	  0,	  0,	  0,	  0,	  0};
	correctArray[7] = new int[correctCols]{0,	  0,	  0,	  0,	  0,	  0};

	m_testNum++;	
	printTestMessage(m_testNum, "MazeWalker finds exit in maze.txt in correct order using bfs");	

	isPassed = testWalker(testFileName, correctArray, correctRows, correctCols, searchChoice);
	
	printPassFail(isPassed);

	for(int i=0; i<correctRows; i++)
	{
		delete[] correctArray[i];
	}

	delete[] correctArray;


	return(isPassed);
}

bool Test::test_mazeWalker03()
{
	bool isPassed = false;
	std::string testFileName = "noExit.txt";
	const int correctRows = 6;
	const int correctCols = 6;
	int** correctArray = new int*[correctRows];
	Search searchChoice = Search::DFS;	

	correctArray[0] = new int[correctCols]{0,	  0,	  0,	  0,	  0,	  1};
	correctArray[1] = new int[correctCols]{7,	  6,	  5,	  4,	  3,	  2};
	correctArray[2] = new int[correctCols]{0,	  0,	  8,	  0,	  0,	  18};
	correctArray[3] = new int[correctCols]{11,	  10,	  9,	  0,	  0,	  19};
	correctArray[4] = new int[correctCols]{0,	  12,	  0,	  0,	  0,	  0};
	correctArray[5] = new int[correctCols]{0,	  13,	  14,	  15,	  16,	  17};

	m_testNum++;	
	printTestMessage(m_testNum, "MazeWalker visits all reachable positions in noExit.txt in correct order using dfs");	

	isPassed = testWalker(testFileName, correctArray, correctRows, correctCols, searchChoice);
	
	printPassFail(isPassed);

	for(int i=0; i<correctRows; i++)
	{
		delete[] correctArray[i];
	}

	delete[] correctArray;


	return(isPassed);
}


bool Test::test_mazeWalker04()
{
	bool isPassed = false;
	std::string testFileName = "noExit.txt";
	const int correctRows = 6;
	const int correctCols = 6;
	int** correctArray = new int*[correctRows];
	Search searchChoice = Search::BFS;	

	correctArray[0] = new int[correctCols]{0,	  0,	  0,	  0,	  0,	  1};
	correctArray[1] = new int[correctCols]{11,	  9,	  7,	  6,	  4,	  2};
	correctArray[2] = new int[correctCols]{0,	  0,	  8,	  0,	  0,	  3};
	correctArray[3] = new int[correctCols]{14,	  12,	  10,	  0,	  0,	  5};
	correctArray[4] = new int[correctCols]{0,	  13,	  0,	  0,	  0,	  0};
	correctArray[5] = new int[correctCols]{0,	  15,	  16,	  17,	  18,	  19};

	m_testNum++;	
	printTestMessage(m_testNum, "MazeWalker visits all reachable positions in noExit.txt in correct order using bfs");	

	isPassed = testWalker(testFileName, correctArray, correctRows, correctCols, searchChoice);
	
	printPassFail(isPassed);

	for(int i=0; i<correctRows; i++)
	{
		delete[] correctArray[i];
	}

	delete[] correctArray;


	return(isPassed);
}
