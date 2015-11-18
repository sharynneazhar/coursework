/**
*	@file : NumberFileDriver.cpp
*	@author : Sharynne Azhar
*	@date : 2015.10.26
*	@brief: Implementation file for number file driver
*/

#include "NumberFileDriver.h"

void NumberFileDriver::run(int argc, char** argv)
{
	std::string flag = argv[2];
	std::string fileName = argv[3];

	int amount = atoi(argv[4]);

	int value = 0;
	int min = 0;
	int max = 0;

	if (argc == 6)
    {
		value = atoi(argv[5]);
	}

	if (argc == 7)
    {
		min = atoi(argv[5]);
		max = atoi(argv[6]);
	}

	if (isValidOption(flag))
    {
		if (flag.compare("-a") == 0)
        {
			NumberFileGenerator::ascending(fileName,amount);
			std::cout << amount << " numbers stored in " << fileName;
		}
		else if (flag.compare("-d") == 0)
        {
			NumberFileGenerator::descending(fileName,amount);
			std::cout << amount << " numbers stored in " << fileName;
		}
		else if (flag.compare("-r") == 0)
        {
			NumberFileGenerator::random(fileName,amount,min,max);
			std::cout << amount << " numbers stored in " << fileName;
		}
		else if (flag.compare("-s") == 0)
        {
			NumberFileGenerator::singleValue(fileName,amount,value);
			std::cout << amount << " numbers stored in " << fileName;
		}
	}
	else
    {
		printHelpMenu();
	}
}

void NumberFileDriver::printHelpMenu()
{
	std::cout << "\nUse Number File Generator in one of the following ways:\n\n"
				<< "./prog -create -a filename amount\n"
				<< "./prog -create -d filename amount\n"
				<< "./prog -create -s filename amount value\n"
				<< "./prog -create -r filename amount min max\n"
				<< "Option explainations:\n"
				<< "\t-a for ascending\n"
				<< "\t-d for descending\n"
				<< "\t-s for a single value\n"
				<< "\t-r for random values\n"
				<< "\tfilename is the ouput file name\n"
				<< "\tamount is the amount of random numbers to place in the file\n"
				<< "\tvalue is the single number that will be written to file in -s mode\n"
				<< "\tmin is the low end of the range of random numbers\n"
				<< "\tmax is the high end (non-inclusive) of the range of random numbers\n";

}

bool NumberFileDriver::isValidOption(std::string option)
{
	return option.compare("-a") == 0  || option.compare("-d") == 0 || option.compare("-s") == 0 || option.compare("-r") == 0;

}
