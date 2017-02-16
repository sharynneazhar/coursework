/*
 ============================================================================
 Author        : Sharynne Azhar
 Date          : 8 February 2017
 Description   : Simulates a train scheduling system using threads
 To Compile    : make; make run
 ============================================================================
*/

#include <iostream>
#include <fstream>
#include <thread>
#include <mutex>

#include "Barrier.h"
#include "Train.h"

Barrier theBarrier;
std::mutex coutMtx;
std::mutex** trackMtxs;

bool ready = false;
int numTrainsLeft;

void runTrain(Train* train) {
  // wait until all trains threads are initialized
  while (!ready) {};

  int timeStep = 0;
  while (!train->isAtEnd()) {

    if (true) { // if track is clear
      std::unique_lock<std::mutex> timeLock(coutMtx);
      std::cout << "At time step " << timeStep << ": ";
      train->travel();

      // trackMtxs[train->getCurrentStop()][train->getNextStop()].unlock();

    } else { // train must stay
      std::unique_lock<std::mutex> timeLock(coutMtx);
      std::cout << "At time step " << timeStep << ": ";
      train->stay();

    }

    // make sure all trains are finished at this time step before moving on
    theBarrier.barrier(numTrainsLeft);

    timeStep++;
  }

  numTrainsLeft--;
}

int main(int argc, char* argv[]) {
  if (argc != 2) {
    std::cerr << "\nMissing file.\n";
    return 0;
  }

  // get information about the number of trains and stations
  int numTrains, numStations;
  std::ifstream file;
  file.open(argv[1]);
  file >> numTrains >> numStations;

  // keep track of how many trains are left for barrier
  numTrainsLeft = numTrains;

  // generate possible pairs of tracks and create a mutex for each
  trackMtxs = new std::mutex*[numStations];
  for (int i = 0; i < numStations; i++) {
    for (int j = 0; j < numStations; j++) {
      std::cout << i << " " << j << "  |  ";
      trackMtxs[i] = new std::mutex();
    }
    std::cout << std::endl;
  }

  // create an array of train threads
  std::thread** trains = new std::thread*[numTrains];

  // get routes for each train
  int numStops, stop;
  for (int i = 0; i < numTrains; i++) {
    file >> numStops;
    int* route = new int[numStops];
    for (int j = 0; j < numStops; j++) {
      file >> route[j];
    }

    // launch threads
    trains[i] = new std::thread(runTrain, new Train(i, numStops, route));
  }

  // Run once all threads are ready
  std::cout << "\nStarting simulation...\n\n";
  ready = true;

  for (int i = 0; i < numTrains; i++) {
    trains[i]->join();
  }

  // All threads are done
  std::cout << "\n\nSimulation complete.\n";

  return 0;
}
