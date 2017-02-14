/*
 ============================================================================
 Author        : Sharynne Azhar
 Last modified : 8 February 2017
 Description   : Simulates a train scheduling system using threads
 To Compile    : make; make run
 ============================================================================
*/

#include <iostream>
#include <fstream>
#include <queue>
#include <thread>
#include <mutex>

#include "Barrier.h"
#include "Train.h"

Barrier theBarrier;
std::mutex coutMutex;

bool ready = false;
int timeStep = 0;

void runTrain(Train* train) {
  // wait until all trains threads are ready
  while (!ready) {};

  if (!train->isAtEnd()) {
    std::unique_lock<std::mutex> mutexLock(coutMutex);

    // if track is clear
    std::cout << "At time step " << timeStep << ": ";
    std::cout << "Train " << train->getId() << " ";
    std::cout << "going from station " << train->getRoute().front() << " ";
    train->goToNextStop();
    std::cout << "to station " << train->getRoute().front() << std::endl;

    // else
    // the train must stay
    // std::cout << "At time step " << timeStep << ": ";
    // std::cout << "Train " << train->getId() << " ";
    // std::cout << "must stay at station " << train->getRoute().front() << " ";
  }
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

  // generate possible pairs of tracks
  // for (int i = 0; i < numStations; i++) {
  //   for (int j = i; j < numStations; j++) {
  //     if (i != j) {
  //       std::cout << i << " " << j << std::endl;
  //     }
  //   }
  // }

  // create a thread for each train
  std::thread** trains = new std::thread*[numTrains];

  // get routes for each train
  int numStops, stop;
  for (int i = 0; i < numTrains; i++) {
    file >> numStops;
    std::queue<int> route;
    for (int j = 0; j < numStops; j++) {
      file >> stop;
      route.push(stop);
    }
    trains[i] = new std::thread(runTrain, new Train(i, route));
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
