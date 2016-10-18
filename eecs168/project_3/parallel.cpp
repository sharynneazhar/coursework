/*
* @author Sharynne Azhar
* @file parallel.cpp
*/

#include <iostream>
#include <fstream>

using namespace std;

const int ORDER_VALUE = 500;
const int MAX_SIZE = 30; 

// loads 3 arrays from file and use count to load cell dimensions 
bool  loadArrays(const char fileName[], int idArray[], int storeArray[], int qtyArray[], int &count, int maxCells);

// prints location of data in array and number of cells filled
void printArrays(ostream &where, const int idArray[], const int storeArray[], const int qtArray[], int count);

// loads data specifically from requestID provided
bool extractData(const char newFileName[], int requestId, int baseQty, const int idArray[], const int storeArray[], const int qtArray[], int count, int &newcount);

int main()
{
	int idProduct[MAX_SIZE];
	int storeNumber[MAX_SIZE];
	int quantity[MAX_SIZE];

	int howMany = 0; // how many cells loaded from original file
	int newCount = 0; // the number of extracted records written to new file
	int searchID; // product ID

	char fileName[MAX_SIZE];
	char newFileName[MAX_SIZE];

	cout << "Enter the name of the file to read: ";
	cin >> fileName;

	// change last parameter to 10 to test where no room for all data
	if (loadArrays(fileName, idProduct, storeNumber, quantity, howMany, 10))
	{
		cout << "All data loaded." << endl;
	}
	else
	{
		cout << "Unable to load all of the records." << endl;
	}

	cout << howMany << " cells loaded." << endl << endl;

	printArrays(cout, idProduct, storeNumber, quantity, howMany);

	cout << endl;
	cout << "Please enter new file name: ";
	cin >> newFileName;
	cout << "Please enter product ID number: ";
	cin >> searchID;

	extractData(newFileName, searchID, ORDER_VALUE, idProduct, storeNumber, quantity, howMany, newCount);
	
	cout << newCount << " cells written to file." << endl;

	return 0;
}

bool  loadArrays(const char fileName[], int idArray[], int storeArray[], int qtyArray[], int &count, int maxCells)
{
	ifstream in;
	in.open(fileName);

	if (!in)
	{
		cout << "Invalid file. Please try again." << endl;
		exit(1); // terminate program
	}

	int i = 0;
	while ((i < maxCells) && (in >> idArray[i] >> storeArray[i] >> qtyArray[i]))
	{
		i++;
	}
	count = i;

	// check if there is more data
	if (in.good())
	{
		return false;
	}
	
	in.close();
	return true;
}

void printArrays(ostream &where, const int idArray[], const int storeArray[], const int qtArray[], int count)
{
	where << "Product\t" << "Store\t" << "Quantity" << endl;
	for (int i = 0; i < count; i++)
	{
		where << idArray[i] << "\t" << storeArray[i] << "\t" << qtArray[i] << endl;
	}
}

bool extractData(const char newFileName[], int requestId, int baseQty, const int idArray[], const int storeArray[], const int qtArray[], int count, int &newcount)
{
	ofstream outfile;
	outfile.open(newFileName);

	if (!outfile.is_open())
	{
		cout << "Unable to open file." << endl;
		return false;
	}

	outfile << "Product ID " << requestId << endl;
	outfile << "Stores\t" << "Quantity" << endl;

	for (int i = 0; i < count; i++)
	{
		if (idArray[i] == requestId && qtArray[i] < baseQty)
		{
			outfile << storeArray[i] << "\t" << qtArray[i] << endl;
			newcount++;
		}
	}

	outfile.close();
	return true;	
}

/*
-----------  First execution ----------------
Enter the name of the file to read: parallel.txt
All data loaded.
20 cells loaded.

Product Store   Quantity
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

Please enter new file name: output.txt
Please enter product ID number: 42424
3 cells written to file.
Press any key to continue . . .

--------- Second execution ------------------
Enter the name of the file to read: parallel.txt
Unable to load all of the records.
10 cells loaded.

Product Store   Quantity
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

Please enter new file name: output2.txt
Please enter product ID number: 42424
1 cells written to file.
Press any key to continue . . .

*/