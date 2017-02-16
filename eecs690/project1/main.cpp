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
std::mutex coutMutex;
std::mutex** trackMtxs;

bool ready = false;
int numTrainsLeft;

void runTrain(Train* train) {
  // wait until all trains threads are initialized
  while (!ready) {};

  int timeStep = 0;
  int* route = train->getRoute();

  for (int i = train->getCurrentStopIdx(); i < train->getNumStops(); i++) {

    coutMutex.lock();

    int currentStation = train->getStation(i);
    int nextStation = train->getStation(i + 1);

    if (true) {
      // if track is clear
      std::cout << "At time step " << timeStep << ": ";
      std::cout << "Train " << train->getId() << " ";
      std::cout << "going from station " << currentStation << " ";
      std::cout << "to station " << nextStation << std::endl;
      //trackMtxs[currentStation][nextStation].unlock();
      train->travel();
    } else {
      // train must stay
      std::cout << "At time step " << timeStep << ": ";
      std::cout << "Train " << train->getId() << " ";
      std::cout << "must stay at station " << currentStation << ".\n";
    }

    coutMutex.unlock();

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

  numTrainsLeft = numTrains;

  // generate possible pairs of tracks
  trackMtxs = new std::mutex*[numStations];
  for (int i = 0; i < numStations; i++) {
    for (int j = i; j < numStations; j++) {
      std::cout << i << " " << j << "  |  ";
      trackMtxs[i] = new std::mutex();
    }
    std::cout << std::endl;
  }

  // create a thread for each train
  std::thread** trains = new std::thread*[numTrains];

  // get routes for each train
  int numStops, stop;
  for (int i = 0; i < numTrains; i++) {
    file >> numStops;
    int* route = new int[numStops];
    for (int j = 0; j < numStops; j++) {
      file >> route[j];
    }
    trains[i] = new std::thread(runTrain, new Train(i, numStops, route));
  }

  std::cout << "\nStarting simulation...\n\n";

  // Run once all threads are ready
  ready = true;

  for (int i = 0; i < numTrains; i++) {
    trains[i]->join();
  }

  // All threads are done
  std::cout << "\n\nSimulation complete.\n";

  return 0;
}
