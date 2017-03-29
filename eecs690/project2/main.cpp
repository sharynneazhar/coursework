/*
 ============================================================================
 Author        : Sharynne Azhar
 Date          : 3 March 2017
 Description   : Parses data from a CSV file
 To Compile    : mpic++ main.cpp -o proj2;
 To Run        : mpirun -np [number of processes] proj2 [-options];
 ============================================================================
*/

#include <algorithm>
#include <fstream>
#include <iostream>
#include <mpi.h>
#include <sstream>
#include <stdlib.h>
#include <string>
#include <vector>

/* Record type represents a single data cell in row or column */
typedef std::vector<std::string> record_t;

/* Columns containing data */
std::string columnNames[117] = {
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
int convert(std::string query) {
  std::string* index = std::find(std::begin(columnNames), std::end(columnNames), query);
  if (index != std::end(columnNames)) {
    return std::distance(columnNames, index);
  }
  return -1;
}

/* Returns the data from a column */
double* getColumn(int column, std::vector<record_t> bigData) {
  if (column != -1) {
    double* data = new double[500];
    for (int i = 0; i < 500; i++) {
      data[i] = std::stod(bigData[i + 1][column]);
    }
    return data;
  }
  return {};
}

/* Find record by value */
record_t getRecord(double value, int column, std::vector<record_t> bigData) {
  for (int i = 0; i < 500; i++) {
    if (std::stod(bigData[i + 1][column]) == value) {
      return bigData[i + 1];
    }
  }
  return {};
}

/* Parses the data from a CSV file */
std::vector<record_t> parseFile() {
  std::vector<record_t> data;
  std::string line;

  std::ifstream file("city-data.csv");

  if (!file) {
    std::cerr << "\nUnable to open file.\n";
    exit(EXIT_FAILURE);
  }

  // parse record columns
  while (std::getline(file, line)) {
    record_t record;
    std::istringstream iss(line);
    std::string field;

    while (std::getline(iss, field, ',')) {
      // take care of ranged columns that have comma in the field
      while (iss && field[0] == '"' && field[field.size() - 1] != '"') {
        std::string next;
        std::getline(iss, next, ',');
        field += ',' + next;
      }

      if (field[0] == '"' && field[field.size() - 1] == '"') {
        field = field.substr(1, field.size() - 2);
      }

      record.push_back(field);
    }

    iss.str("");
    data.push_back(record);
  }

  file.close();

  return data;
}

/* Returns the maximum of an array of numbers */
double findMax(double currMax, double* values, int size) {
  for (int i = 0; i < size; i++) {
    if (values[i] > currMax) {
      currMax = values[i];
    }
  }
  return currMax;
}

/* Returns the minimum of an array of numbers */
double findMin(double currMin, double* values, int size) {
  for (int i = 0; i < size; i++) {
    if (values[i] < currMin) {
      currMin = values[i];
    }
  }
  return currMin;
}

/* Returns the average of an array of numbers */
double findAvg(double* values, int size) {
  double sum = 0;
  for (int i = 0; i < size; i++) {
    sum += values[i];
  }
  return sum / (double) 500;
}

/* Returns the number of elements that satisfies the filter */
double filter(std::string filter, double filterValue, double* values, int size) {
  double count = 0;
  if (filter == "gt") {
    for (int i = 0; i < size; i++) {
      if (values[i] > filterValue) {
        count++;
      }
    }
  } else {
    for (int i = 0; i < size; i++) {
      if (values[i] < filterValue) {
        count++;
      }
    }
  }
  return count;
}

/* Main */
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
                << "\n\t[operation] - min, max, avg, or number (sr only)"
                << "\n\t[column] - data column in file"
                << "\n\t[conditional] - lt or gt (optional)\n";
    }
    return 0;
  }

  // check communicator size
  if ((500 % communicatorSize) != 0) {
		if (rank == 0) {
			std::cerr << "\nThe communicatorSize does not evenly divide 500\n";
    }
    return 0;
	}

  // read arguments
  std::string strategyArg = argv[1];
  std::string operationArg = argv[2];
  std::string conditionalArg;
  double conditionalValue;

  // parse file
  std::vector<record_t> data = parseFile();

  // either "scatter–reduce" (sr) or "broadcast–gather" (bg)
  if (strategyArg == "sr") {
    int rowsToCompute = 500 / communicatorSize;       // number of rows each process will receive
    int colIndex = convert(argv[3]);                  // the column index queried
    double* columnData = getColumn(colIndex, data);   // the entire column queried
    double columnDataChunk[rowsToCompute];            // the chunk of data each process will receive
    double resultFromProcesses = 0;                   // the result retreived from each process
    double finalResult = 0;                           // the final result sent to the rank 0 process

    MPI_Scatter(columnData, rowsToCompute, MPI_DOUBLE,
                columnDataChunk, rowsToCompute, MPI_DOUBLE,
                0, MPI_COMM_WORLD);

    if (operationArg == "max") {
      resultFromProcesses = findMax(columnData[0], columnDataChunk, rowsToCompute);
      MPI_Reduce(&resultFromProcesses, &finalResult, 1, MPI_DOUBLE, MPI_MAX, 0, MPI_COMM_WORLD);
    } else if (operationArg == "min") {
      resultFromProcesses = findMin(columnData[0], columnDataChunk, rowsToCompute);
      MPI_Reduce(&resultFromProcesses, &finalResult, 1, MPI_DOUBLE, MPI_MIN, 0, MPI_COMM_WORLD);
    } else if (operationArg == "avg") {
      resultFromProcesses = findAvg(columnDataChunk, rowsToCompute);
      MPI_Reduce(&resultFromProcesses, &finalResult, 1, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);
    } else {
      conditionalArg = (argv[4] != NULL) ? argv[4] : "none";
      conditionalValue = (argv[4] != NULL && argv[5] != NULL) ? std::stod(argv[5]) : 0;
      resultFromProcesses = filter(conditionalArg, conditionalValue, columnDataChunk, rowsToCompute);
      MPI_Reduce(&resultFromProcesses, &finalResult, 1, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);
    }

    if (rank == 0) {
      if (operationArg == "max" || operationArg == "min") {
        record_t record = getRecord(finalResult, colIndex, data);
        std::cout << record[1] << ", " << record[0] << " "
                  << data[0][colIndex] << " = "
                  << finalResult << std::endl;
      } else if (operationArg == "avg") {
        std::cout << "Average " << data[0][colIndex] << " = "
                  << finalResult << std::endl;
      } else {
        record_t record = getRecord(finalResult, colIndex, data);
        std::cout << "Number cities with " << data[0][colIndex] << " "
                  << conditionalArg << " " << conditionalValue << " = "
                  << finalResult << std::endl;
      }
    }
  } else {
    // The number of processes is the same as the number of columns to be examined
    int totalCol = communicatorSize;

    std::string columnArgs[totalCol];       // the columns being queried
    double columnData[totalCol][500];       // array of columns to process
    double resultFromProcesses[totalCol];   // array of results retrieved from each process
    double resultFromProcess;               // results retrieved from a single process
    double finalResult;                     // the final result send to the rank 0 process

    if (rank == 0) {
      for (int i = 0; i < totalCol; i++) {
        columnArgs[i] = argv[i + 3]; // column info starts at argv[3]
      }

      for (int i = 0; i < totalCol; i++) {
        double* col = getColumn(convert(columnArgs[i]), data);
        for (int j = 0; j < 500; j++) {
          columnData[i][j] = col[j];
        }
      }
    }

    MPI_Bcast(&columnData[0][0], 500 * totalCol, MPI_DOUBLE, 0, MPI_COMM_WORLD);

    if (operationArg == "max") {
      resultFromProcess = columnData[rank][0];
      for (int i = 0; i < 500; i++) {
        if (columnData[rank][i] > resultFromProcess) {
          resultFromProcess = columnData[rank][i];
        }
      }
    } else if (operationArg == "min") {
      resultFromProcess = columnData[rank][0];
      for (int i = 0; i < 500; i++) {
        if (columnData[rank][i] < resultFromProcess) {
          resultFromProcess = columnData[rank][i];
        }
      }
    } else {
      for (int i = 0; i < 500; i++) {
        resultFromProcess += columnData[rank][i] / (double) 500;
      }
    }

    MPI_Gather(&resultFromProcess, 1, MPI_DOUBLE,
               resultFromProcesses, 1, MPI_DOUBLE,
               0, MPI_COMM_WORLD);

    if (rank == 0) {
      if (operationArg == "max" || operationArg == "min") {
        for (int i = 0; i < totalCol; i++) {
          int colIndex = convert(columnArgs[i]);
          record_t record = getRecord(resultFromProcesses[i], colIndex, data);
          std::cout << operationArg << " "
                    << data[0][colIndex] << " = "
                    << resultFromProcesses[i] << "; "
                    << record[1] << ", " << record[0] << std::endl;
        }
      } else {
        for (int i = 0; i < totalCol; i++) {
          int colIndex = convert(columnArgs[i]);
          std::cout << "Average " << data[0][colIndex] << " = "
                    << resultFromProcesses[i] << std::endl;
        }
      }
    }
  }

  // terminates the MPI execution environment
	MPI_Finalize();

	return 0;
}
