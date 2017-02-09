/*
 ============================================================================
 Author        : Sharynne Azhar
 Last modified : 8 February 2017
 Description   : Simulates a train scheduling system using threads
 ============================================================================
*/

#include <iostream>
#include <fstream>
#include <thread>
#include <stdlib.h>

int main(int argc, char* argv[]) {

  if (argc != 2) {
    std::cerr << "\nMissing file.\n";
    exit(EXIT_FAILURE);
  }

  int numTrains;
  int numStations;

  std::ifstream file;
  file.open(argv[1]);

  int numTrains;
  int numStations;

  file >> numTrains >> numStations;


  return 0;
}
