/*
 ============================================================================
 Author        : Sharynne Azhar
 Date          : 3 March 2017
 Description   : Parses data from a CSV file
 To Compile    : mpic++ main.cpp -o proj2;
 To Run        : mpirun -np [number of processes] proj2;
 ============================================================================
*/

#include <algorithm>
#include <fstream>
#include <math.h>
#include <iostream>
#include <mpi.h>
#include <sstream>
#include <stdlib.h>
#include <string>
#include <vector>

const int rows = 500;
const int cols = 117;

/* Record type represents a single data cell in row orcolumn */
typedef std::vector<std::string> record_t;

/* Columns containing data */
std::string columnNames[cols] = {
   "A",  "B",  "C",  "D",  "E",  "F",  "G",  "H",  "I",  "J",  "K",  "L",  "M",
   "N",  "O",  "P",  "Q",  "R",  "S",  "T",  "U",  "V",  "W",  "X",  "Y",  "Z",
  "AA", "AB", "AC", "AD", "AE", "AF", "AG", "AH", "AI", "AJ", "AK", "AL", "AM",
  "AN", "AO", "AP", "AQ", "AR", "AS", "AT", "AU", "AV", "AW", "AX", "AY", "AZ",
  "BA", "BB", "BC", "BD", "BE", "BF", "BG", "BH", "BI", "BJ", "BK", "BL", "BM",
  "BN", "BO", "BP", "BQ", "BR", "BS", "BT", "BU", "BV", "BW", "BX", "BY", "BZ",
  "CA", "CB", "CC", "CD", "CE", "CF", "CG", "CH", "CI", "CJ", "CK", "CL", "CM",
  "CN", "CO", "CP", "BQ", "CR", "CS", "CT", "CU", "CV", "CW", "CX", "CY", "CZ",
  "DA", "DB", "DC", "DD", "DE", "DF", "DG", "DH", "DI", "DJ", "DK", "DL", "DM"
};

/* Finds the index of a given column key */
int findColumn(std::string key) {
  std::string* index = std::find(std::begin(columnNames), std::end(columnNames), key);
  if (index != std::end(columnNames))
    return std::distance(columnNames, index);
  return -1;
}

/* Parses the data from a CSV file */
std::vector<record_t> parseFile(std::ifstream& file) {
  std::vector<record_t> data;
  record_t record;

  // parse rows
  while (file) {
    std::string tmp;

    if (!std::getline(file, tmp))
      break;

    std::istringstream line(tmp);

    // parse record columns
    while (line) {
      std::string field;

      if (!std::getline(line, field, ','))
        break;

      // take care of ranged columns that have comma in the field
      while (line && field[0] == '"' && field[field.size() - 1] != '"') {
        std::string next;
        std::getline(line, next, ',');
        field += ',' + next;
      }

      if (field[0] == '"' && field[field.size() - 1] == '"') {
        field = field.substr(1, field.size() - 2);
      }

      record.push_back(field);
    }

    data.push_back(record);
  }

  return data;
}

/* Stores all the data and handles all standard output commands */
void do_rank_0_work(int communicatorSize, int rowsToCompute, std::string queries[]) {
  // the rank 0 process must read the CSV file and store all the data
  // both reading the file and writing results to standard output MUST be done here
  std::ifstream file("city-data.csv");

  if (!file) {
    std::cerr << "\nUnable to open file.\n";
    exit(EXIT_FAILURE);
  }

  // parse file and store data
  std::cout << "\nPreparing data...\n";
  std::vector<record_t> data = parseFile(file);

  // read arguments
  std::string strategy = queries[0];
  std::string query = queries[1];
  std::string column = queries[2];
  std::string conditional = queries[3];

  // either "scatter–reduce" (sr) or "broadcast–gather" (bg)
  std::cout << "\nDistributing data to other processes...\n";
  if (strategy == "sr") {
    // scatter the data to respective processes
    // std::cout << "\nColumn Index: " << findColumn(column);

  } else {


  }

  if (!file && !file.eof()) {
    std::cout << "\nEncountered a problem reading file.\n" << std::endl;
    exit(EXIT_FAILURE);
  }

  file.close();
}

void do_rank_i_work() {

}

int main (int argc, char **argv) {
  // initializes the MPI execution environment
	MPI_Init(&argc, &argv);

	int rank, communicatorSize;
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Comm_size(MPI_COMM_WORLD, &communicatorSize);

  // get the desired queries
  if (argc < 4) {
    if (rank == 0) {
      std::cerr << "\nMissing arguments. "
                << "\nArguments required: "
                << "\n\t[strategy] - sr or bg"
                << "\n\t[query] - min, max, avg, or number (sr only)"
                << "\n\t[column] - data column in file"
                << "\n\t[conditional] - lt or gt (optional)\n";
    }
  } else {
    std::string conditional = (argv[4] != NULL) ? argv[4] : "none";
    std::string queries[4] = { argv[1], argv[2], argv[3], conditional };

    // start process
    if ((rows % communicatorSize) != 0) {
  		if (rank == 0) {
  			std::cerr << "\nThe communicatorSize " << communicatorSize
                  << " does not evenly divide 500\n";
      }
  	} else {
  		int rowsToCompute = rows / communicatorSize;
      if (rank == 0) {
        do_rank_0_work(communicatorSize, rowsToCompute, queries);
      } else {
        do_rank_i_work();
      }
  	}
  }

  // terminates the MPI execution environment
	MPI_Finalize();

	return 0;
}
