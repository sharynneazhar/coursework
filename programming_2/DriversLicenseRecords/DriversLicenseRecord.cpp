/**
*	@file : DriversLicenseRecord.cpp
*	@author : Sharynne Azhar
*	@date : 02-01-2016
*	Purpose: Implementation file for the DriversLicenseRecord class
*/

#include "DriversLicenseRecord.h"

DriversLicenseRecord::DriversLicenseRecord()
{
    m_firstName = "";
    m_lastName = "";
    m_isRegistered = 'N';
    m_age = 0;
    m_licenseNumber = 0;
}

DriversLicenseRecord::DriversLicenseRecord(std::string firstName, std::string lastName, int age, char isRegistered, int licenseNumber)
{
    m_firstName = firstName;
    m_lastName = lastName;
    m_isRegistered = isRegistered;
    m_age = age;
    m_licenseNumber = licenseNumber;
}

std::string DriversLicenseRecord::getFirstName() const
{
    return m_firstName;
}

void DriversLicenseRecord::setFirstName(std::string firstName)
{
    m_firstName = firstName;
}

std::string DriversLicenseRecord::getLastName() const
{
    return m_lastName;
}

void DriversLicenseRecord::setLastName(std::string lastName)
{
    m_lastName = lastName;
}

char DriversLicenseRecord::getIsRegistered() const
{
    return m_isRegistered;
}

void DriversLicenseRecord::setIsRegistered(char isRegistered)
{
    m_isRegistered = isRegistered;
}

int DriversLicenseRecord::getAge() const
{
    return m_age;
}

void DriversLicenseRecord::setAge(int age)
{
    m_age = age;
}

int DriversLicenseRecord::getLicenseNumber() const
{
    return m_licenseNumber;
}

void DriversLicenseRecord::setLicenseNumber(int licenseNumber)
{
    m_licenseNumber = licenseNumber;
}

void DriversLicenseRecord::printRecords(int count)
{
    std::cout << "\nRecord #" << count << ": "
              << m_firstName << " " << m_lastName
              << " -- Age: " << m_age
              << " -- Registered? " << m_isRegistered
              << " -- License Number: " << m_licenseNumber;
}
