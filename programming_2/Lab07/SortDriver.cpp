/**
*	@file : SortDriver.cpp
*	@author : Sharynne Azhar
*	@date : 2015.10.26
*	@brief: Implementation file for sort driver
*/

#include "SortDriver.h"

void SortDriver::run(int argc, char** argv)
{
	std::string sortType = argv[2];
	std::string inputFileName = argv[3];
	std::string outputFileName = argv[4];

	if(areParametersValid(sortType,inputFileName))
	{
		std::ifstream input(inputFileName);

		int amount = getFileCount(input);

		int* arr = createArray(input,amount);

		std::cout << "Calculating sort timing information...\n";

		double time0 = 0.0;
		double time1 = 0.0;
		double time2 = 0.0;
		double time3 = 0.0;
		double time4 = 0.0;
		double time5 = 0.0;

		if (sortType == "-bubble")
		{
			time0 = Sorts<int>::sortTimer(Sorts<int>::bubbleSort,arr,amount);
		}
		else if( sortType == "-insertion")
		{
			time0 = Sorts<int>::sortTimer(Sorts<int>::insertionSort,arr,amount);
		}
		else if (sortType == "-selection")
		{
			time0 = Sorts<int>::sortTimer(Sorts<int>::selectionSort,arr,amount);
		}
		else if (sortType == "-quick")
		{
			time0 = Sorts<int>::sortTimer(Sorts<int>::quickSort,arr,amount);
		}
		else if (sortType == "-quick3")
		{
			time0 = Sorts<int>::sortTimer(Sorts<int>::quickSortWithMedian,arr,amount);
		}
		else if (sortType == "-merge")
		{
			time0 = Sorts<int>::sortTimer(Sorts<int>::mergeSort,arr,amount);
		}
		else if (sortType == "-all")
		{
			int* copy = new int[amount];

			copyArray(arr,copy,amount);

			time0 = Sorts<int>::sortTimer(Sorts<int>::bubbleSort,copy,amount);

			delete [] copy;
			copy = new int[amount];

			copyArray(arr,copy,amount);

			time1 = Sorts<int>::sortTimer(Sorts<int>::selectionSort,copy,amount);

			delete [] copy;
			copy = new int[amount];

			copyArray(arr,copy,amount);

			time2 = Sorts<int>::sortTimer(Sorts<int>::insertionSort,copy,amount);

			delete [] copy;
			copy = new int[amount];

			copyArray(arr,copy,amount);

			time3 = Sorts<int>::sortTimer(Sorts<int>::mergeSort,copy,amount);

			delete [] copy;
			copy = new int[amount];

			copyArray(arr,copy,amount);

			time4 = Sorts<int>::sortTimer(Sorts<int>::quickSort,copy,amount);

			delete [] copy;
			copy = new int[amount];

			copyArray(arr,copy,amount);

			time5 = Sorts<int>::sortTimer(Sorts<int>::quickSortWithMedian,copy,amount);

			delete [] copy;
			copy = nullptr;

		}
		else
		{
			std::cout<<"Invalid flag";
		}

		std::cout << "Calculations finished.  Results stored in " << outputFileName << ".\n";

		std::ofstream output(outputFileName);

		if (sortType == "-bubble")
		{
			output << "bubble " <<amount << " " << time0 << "\n";
		}
		else if (sortType == "-selection")
		{
			output << "selection " << amount << " " << time0 << "\n";

		}
		else if (sortType == "-insertion")
		{
			output << "insertion " << amount << " " << time0 << "\n";

		}
		else if (sortType == "-merge")
		{
			output << "merge " << amount << " " << time0 << "\n";

		}
		else if (sortType == "-quick")
		{
			output << "quick " << amount << " " << time0 << "\n";

		}
		else if (sortType == "-quick3")
		{
			output << "quick3 " << amount << " " << time0 << "\n";

		}
		else if (sortType == "-all")
		{
			output << "bubble " << amount << " " << time0 << "\n";
			output << "selection " << amount << " " << time1 << "\n";
			output << "insertion " << amount << " " << time2 << "\n";
			output << "merge " << amount << " " << time3 << "\n";
			output << "quick " << amount << " " << time4 << "\n";
			output << "quick3 " << amount << " " << time5 << "\n";
		}

		input.close();
		output.close();

		delete [] arr;
		arr = nullptr;
	}
	else
	{
		printHelpMenu();
	}
}

void SortDriver::printHelpMenu()
{
	std::cout << "\nSorting is done one of the following ways:\n\n"
			<< "./prog -sort -bubble inputFile outputFile\n"
			<< "./prog -sort -selection inputFile outputFile\n"
			<< "./prog -sort -insertion inputFile outputFile\n"
			<< "./prog -sort -quick inputFile outputFile\n"
			<< "./prog -sort -quick3 inputFile outputFile\n"
			<< "./prog -sort -merge inputFile outputFile\n"
			<< "./prog -sort -all inputFile outputFile\n"
			<< "Option explainations:\n"
			<< "\t-bubble to time bubble sort and store all timing results in outputFile\n"
			<< "\t-selection to time selection sort and store all timing results in outputFile\n"
			<< "\t-insertion to time insertion sort and store all timing results in outputFile\n"
			<< "\t-quick to time quick sort and store all timing results in outputFile\n"
			<< "\t-quick3 to time quick3 sort  and store all timing results in outputFile\n"
			<< "\t-merge to time merge sort  and store all timing results in outputFile\n"
			<< "\t-all to time all of the sorts and store all timing results in outputFile\n"
			<< "\tinputFile must be file created by a NumberFileGenerator\n"
			<< "\toutputFile must be to a valid path. It will hold the timing results\n";
}

bool SortDriver::isFileAccessible(std::string fileName)
{
	std::ifstream file(fileName);

	if (file.good())
    {
        return true;
    }

    return false;
}

bool SortDriver::isSortValid(std::string sortParameter)
{
    if (sortParameter.compare("-bubble") == 0)
    {
		return true;
	}

	if (sortParameter.compare("-selection") == 0)
    {
		return true;
	}

	if (sortParameter.compare("-insertion") == 0)
    {
		return true;
	}

	if (sortParameter.compare("-quick") == 0)
    {
		return true;
	}

	if (sortParameter.compare("-quick3") == 0)
    {
		return true;
	}

	if (sortParameter.compare("-merge") == 0)
    {
		return true;
	}

	if (sortParameter.compare("-all") == 0)
    {
		return true;
	}

    return false;
}

bool SortDriver::areParametersValid(std::string sortName, std::string inputFileName)
{
	return isSortValid(sortName) && isFileAccessible(inputFileName);
}

int SortDriver::getFileCount(std::ifstream& inputFile)
{
	int count = 0;
	inputFile >> count;
	return count;
}

int* SortDriver::createArray(std::ifstream& inputFile, int size)
{
	int* arr = new int[size];
	for(int i = 0; i < size; i++)
    {
		inputFile >> arr[i];
	}

	return arr;
}

void SortDriver::copyArray(int original[], int copy[], int size)
{
	for(int i = 0; i < size; i++)
    {
		copy[i] = original[i];
	}
}
