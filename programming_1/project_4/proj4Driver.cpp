/**
* @author Sharynne Azhar
* @file proj4Driver.cpp
*/

#include <iostream>
#include <fstream>
#include "inventory.h"

const int ORDER_VALUE = 500; // quantity value used to extract data
const int MAX_ARR_SIZE = 25; // max array size
const int MAX_FILENAME_LEN = 40; // max length for filename string

// will load data from the disk file and returns true if all data are loaded
bool loadArray(char fileName[], inventory invList[], int & count, int maxCells);

// prints the data in the array of objects to the stream "where"
void printArray(ostream & where, const inventory invList[], int count);

// will write all data with matching requestId and under quantity of baseQty to new file
bool extractData(char newFileName[], int requestId, int baseQty,
	const inventory invList[], int count, int & newcount);

int main()
{
	inventory invList[MAX_ARR_SIZE];

	int count = 0; // holds the number of cells found in original file
	int newcount = 0; // hold the number of extracted records written to new file
	int requestId;

	char fileName[MAX_FILENAME_LEN];
	char newFile[MAX_FILENAME_LEN];

	cout << "Enter the name of the file to be read: ";
	cin >> fileName; // inventory.txt

	// change last parameter to 10 to test when no room for all data
	if (loadArray(fileName, invList, count, MAX_ARR_SIZE))
	{
		cout << "All data loaded." << endl;
	}
	else
	{
		cout << "Insufficent storage. Unable to load all data." << endl;
	}

	cout << count << " record(s) found" << endl << endl;
	printArray(cout, invList, count);

	cout << endl;
	cout << "Enter new file name: ";
	cin >> newFile;
	cout << "Enter product ID to search for: ";
	cin >> requestId; // product id number

	extractData(newFile, requestId, ORDER_VALUE, invList, count, newcount);
	cout << newcount << " record(s) written to file." << endl << endl;

	return 0;
}

void printArray(ostream & where, const inventory invList[], int count)
{
	for (int i = 0; i < count; i++)
	{
		where << invList[i].getId() << "\t" 
			<< invList[i].getStoreNr() << "\t" 
			<< invList[i].getQuantity() << endl;
	}
}

bool loadArray(char fileName[], inventory invList[], int & count, int maxCells)
{
	ifstream file;
	file.open(fileName); 

	if (!file)
	{
		cout << "Unable to open file." << endl;
		exit(1); // terminate program
	}

	count = 0;
	for (int i = 0; i < maxCells; i++)
	{
		int prod, store, qty; // temp variables
		file >> prod >> store >> qty;
		invList[i].setId(prod);
		invList[i].setStoreNr(store);
		invList[i].setQuantity(qty);
		count++; // tracks the number of cells loaded
	}

	// check if there is more data
	if (file.good())
	{
		return false;
	}

	file.close();

	return true;
}

bool extractData(char newFileName[], int requestId, int baseQty, 
	const inventory invList[], int count, int & newcount)
{
	ofstream outfile;
	outfile.open(newFileName);

	if (!outfile.is_open())
	{
		cout << "Unable to open file." << endl;
		return false;
	}

	newcount = 0;
	for (int i = 0; i < count; i++)
	{
		if (invList[i].getId() == requestId && invList[i].getQuantity() < baseQty)
		{
			outfile << invList[i].getId() << "\t"
				<< invList[i].getStoreNr() << "\t"
				<< invList[i].getQuantity() << endl;
			newcount++;
		}
	}

	outfile.close();

	return true;
}

/*
--------------------------------
------- All data loaded --------
--------------------------------

Enter the name of the file to be read: inventory.txt
All data loaded.
25 record(s) found.

34230   1425    46
24098   1425    94
13133   1425    12
23246   1090    23
59324   1090    58
13133   3802    78
13133   3802    47
83473   3802    38
42424   3802    43
35020   1314    85
34230   1314    10
42424   1458    0
59324   1458    66
24098   3529    53
83473   3529    58
64837   5032    18
34230   5032    22
23246   5032    30
74537   4585    14
42424   4585    25
42424   4585    25
42424   4585    25
42424   4585    25
42424   4585    25
42424   4585    25

Enter new file name: all_data_loaded.txt
Enter ID to search for: 42424
8 record(s) written to file.

Press any key to continue . . .

--------------------------------
----- No room for all data -----
--------------------------------

Enter the name of the file to be read: inventory.txt
Insufficent storage. Unable to load all data.
10 record(s) found

34230   1425    46
24098   1425    94
13133   1425    12
23246   1090    23
59324   1090    58
13133   3802    78
13133   3802    47
83473   3802    38
42424   3802    43
35020   1314    85

Enter new file name: no_room.txt
Enter product ID to search for: 13133
3 record(s) written to file.

Press any key to continue . . .
*/