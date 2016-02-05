/**
*	@file : Executive.cpp
*	@author : Sharynne Azhar
*	@date : 02-01-2016
*	Purpose: Implementation file for the Executive class
*/

#include "Executive.h"

Executive::Executive(const std::string file) throw (ExecutiveReaderException)
{
    dlrFile.open(file);

    if (dlrFile.fail()) // check if file is valid
    {
        throw ExecutiveReaderException("Error. Bad file.");
    }

    if (dlrFile.peek() == std::ifstream::traits_type::eof())
    {
        throw ExecutiveReaderException("Error. Empty file");
    }

    dlrFile >> numRecords; // get the number of records in the file

    // read data in array
    dlrArray = new DriversLicenseRecord[numRecords];
    readFile();
}

Executive::~Executive()
{
    delete [] dlrArray;
}

void Executive::readFile()
{
    std::string fn, ln;
    char isRegistered;
    int age, licenseNumber;

    // read data from file into dynamic array
    for (int i = 0; i < numRecords; i++)
    {
        dlrFile >> fn >> ln >> age >> isRegistered >> licenseNumber;
        dlrArray[i] = DriversLicenseRecord(fn, ln, age, isRegistered, licenseNumber);
    }

    dlrFile.close();
}

void Executive::printMenu()
{
    std::cout << "\n\n================\n";
    std::cout << "      Menu      \n";
    std::cout << "================\n";
    std::cout << "1. Query last name\n";
    std::cout << "2. Query age range\n";
    std::cout << "3. Query registered voters\n";
    std::cout << "4. Quit";
}

void Executive::findByLastName(std::string lastName, DriversLicenseRecord* arr, int size) throw (ExecutiveReaderException)
{
    std::string temp;
    int numFound = 0;

    for (int i = 0; i < size; i++)
    {
        temp = arr[i].getLastName();
        if (temp == lastName)
        {
            numFound++;
            arr[i].printRecords(numFound);
        }
    }

    if (numFound == 0)
    {
        throw ExecutiveReaderException("Last name not found.");
    }
}

void Executive::findByAge(int age_min, int age_max, DriversLicenseRecord* arr, int size) throw (ExecutiveReaderException)
{
    int temp;
    int numFound = 0;

    for (int i = 0; i < size; i++)
    {
        temp = arr[i].getAge();
        if ((temp >= age_min) && (temp <= age_max))
        {
            numFound++;
            arr[i].printRecords(numFound);
        }
    }

    if (numFound == 0)
    {
        throw ExecutiveReaderException("No person(s) within age range");
    }
}

void Executive::findByRegisteredVoters(char isRegistered, DriversLicenseRecord* arr, int size) throw (ExecutiveReaderException)
{
    int temp;
    int numFound = 0;

    for (int i = 0; i < size; i++)
    {
        temp = arr[i].getIsRegistered();
        if (temp == 'Y')
        {
            numFound++;
            arr[i].printRecords(numFound);
        }
    }

    if (numFound == 0)
    {
        throw ExecutiveReaderException("No registered voters found.");
    }
}

void Executive::run()
{
    bool done = false;
    int menuChoice;

    do
    {
        printMenu();
        std::cout << "\n\nYour choice: ";
        std::cin >> menuChoice;

        switch (menuChoice)
        {
        case 1:
        {
            std::string lastName;
            std::cout << "Enter last name: ";
            std::cin >> lastName;
            lastName[0] = toupper(lastName[0]);
            try
            {
                findByLastName(lastName, dlrArray, numRecords);
            }
            catch (ExecutiveReaderException& e)
            {
                std::cout << e.what();
            }
            break;
        }
        case 2:
        {
            int age_min, age_max;
            std::cout << "Enter age range (separated by single space): ";
            std::cin >> age_min >> age_max;
            try
            {
                findByAge(age_min, age_max, dlrArray, numRecords);
            }
            catch (ExecutiveReaderException& e)
            {
                std::cout << e.what();
            }
            break;
        }
        case 3:
        {
            std::cout << "\nCompiling a list of registered voters...\n";
            try
            {
                findByRegisteredVoters('Y', dlrArray, numRecords);
            }
            catch (ExecutiveReaderException& e)
            {
                std::cout << e.what();
            }
            break;
        }
        case 4:
        {
            done = true;
            break;
        }
        default:
        {
            std::cout << "Invalid input. Please try again.";
            std::cin.clear(); // clear the input stream
            std::cin.ignore(256, '\n'); // ignore any remaining buffers
            done = false;
        }
        } // end switch
    } while (!done);

    std::cout << "\nThanks for using our system!\n";
    std::cout << "Bye.\n\n";
}
