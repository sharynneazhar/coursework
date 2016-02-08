/**
*	@file : DriversLicenseRecord.h
*	@author : Sharynne Azhar
*	@date : 02-01-2016
*	Purpose: header file for the DriversLicenseRecord class that is used to store
*            the data in an array
*/

#ifndef DRIVERSLICENSERECORD_H
#define DRIVERSLICENSERECORD_H

#include <iostream>
#include <string>

class DriversLicenseRecord
{
private:
    std::string m_firstName;
    std::string m_lastName;
    char m_isRegistered;
    int m_age;
    int m_licenseNumber;

public:
    /*
    * @post creates an instance of DriversLicenseRecord to store data
    */
    DriversLicenseRecord();

    /*
    * @pre expects all parameters to be correct type and valid
    * @post creates an instance of DriversLicenseRecord of given data
    */
    DriversLicenseRecord(std::string firstName, std::string lastName, int age, char isRegistered, int licenseNumber);

    /*
    * @returns first name
    */
    std::string getFirstName() const;

    /*
    * @pre valid first name passed in
    * @post set the client's first name to data given
    */
    void setFirstName(std::string firstName);

    /*
    * @returns last name
    */
    std::string getLastName() const;

    /*
    * @pre valid last name passed in
    * @post set the client's last name to data given
    */
    void setLastName(std::string lastName);

    /*
    * @returns registration status
    */
    char getIsRegistered() const;

    /*
    * @pre valid registration status passed in
    * @post set the client's registration status to data given
    */
    void setIsRegistered(char isRegistered);

    /*
    * @returns age
    */
    int getAge() const;

    /*
    * @pre valid age passed in
    * @post set the client's age to data given
    */
    void setAge(int age);

    /*
    * @returns license number
    */
    int getLicenseNumber() const;

    /*
    * @pre valid license number passed in
    * @post set the client's  license number to data given
    */
    void setLicenseNumber(int licenseNumber);

    /*
    * @post prints client records to console
    */
    void printRecords(int count);
};

#endif
