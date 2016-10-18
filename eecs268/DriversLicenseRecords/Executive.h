/**
*	@file : Executive.h
*	@author : Sharynne Azhar
*	@date : 02-01-2016
*	Purpose: header file for the Executive class  used to coordinate the reading
*            of the data into the array
*/

#ifndef EXECUTIVE_H
#define EXECUTIVE_H

#include <iostream>
#include <fstream>
#include <string>
#include "DriversLicenseRecord.h"
#include "ExecutiveReaderException.h"

class Executive
{
private:
    int numRecords;
    std::ifstream dlrFile;
    DriversLicenseRecord* dlrArray;

public:
    /*
    * @pre expects user provide a file to read
    * @post uses readFile() to create an array of records
    * @throws when fails to open file or file empty
    */
    Executive(const std::string file) throw (ExecutiveReaderException);

    /*
    * @post garbage collector -- deletes data from heap
    */
    ~Executive();

    /*
    * @pre assumes a valid and properly formatted file
    * @post stores data into the array
    */
    void readFile();

    /*
    * @post prints top menu to console
    */
    void printMenu();

    /*
    * @pre assumes valid array with size and last name provided
    * @post prints list of records found to console
    * @throws when last name not found
    */
    void findByLastName(std::string lastName, DriversLicenseRecord* arr, int size);

    /*
    * @pre assumes valid array with size and age range provided
    * @post prints list of records found to console
    * @throws when age is out of range
    */
    void findByAge(int age_min, int age_max, DriversLicenseRecord* arr, int size);

    /*
    * @pre assumes valid array with size
    * @post prints list of records found to console
    * @throws when registered voters not found
    */
    void findByRegisteredVoters(char isRegistered, DriversLicenseRecord* arr, int size);

    /*
    * @pre assumes all instances and methods are valid
    * @post processes the queries chose by user
    */
    void run();

};

#endif
