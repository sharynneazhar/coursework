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

#include <iomanip>

#include "Barrier.h"
#include "Train.h"

struct Track {
  int stationI;
  int stationJ;
};

Barrier theBarrier;
Track** tracks;
std::mutex coutMtx;

bool ready = false;

void runTrain(Train* train, int numTrains) {
  int timeStep = 0;

  // wait until all trains threads are initialized
  while (!ready) {};

  while (timeStep != 12) {
    if (!train->isAtEnd()) {
      // if track is clear (mutex lockable) advance, else stay
      coutMtx.lock();

      if (true) {
        std::cout << "At time step " << timeStep << ": ";
        train->travel();
      } else {
        std::cout << "At time step " << timeStep << ": ";
        train->stay();
      }

      coutMtx.unlock();
    }

    // make sure all trains are finished at this time step before moving on
    theBarrier.barrier(numTrains);
    timeStep++;
  }

}

int main(int argc, char* argv[]) {

  // get information about the number of trains and stations
  int numTrains;
  int numStations;
  int numStops;

  std::ifstream file;
  file.open((argc == 2) ? argv[1] : "routes.txt");
  file >> numTrains >> numStations;

  // initialize all the trains
  Train** trains = new Train*[numTrains];
  for (int i = 0; i < numTrains; i++) {
    file >> numStops;
    int* route = new int[numStops];
    for (int j = 0; j < numStops; j++) {
      file >> route[j];
    }
    trains[i] = new Train(i, numStops, route);
  }

  file.close();

  // initialize tracks - since there are 6 trains,
  // there would only be 6 tracks at any given time
  tracks = new Track*[numTrains];
  for (int i = 0; i < numTrains; i++) {
    tracks[i] = nullptr;
  }

  // create a thread for each train
  std::thread** trainThreads = new std::thread*[numTrains];
  for (int i = 0; i < numTrains; i++) {
    trainThreads[i] = new std::thread(runTrain, trains[i], numTrains);
  }

  // Run once all threads are ready
  std::cout << "\nStarting simulation...\n\n";
  ready = true;

  for (int i = 0; i < numTrains; i++) {
    trainThreads[i]->join();
  }

  // All threads are done
  std::cout << "\n\nSimulation complete.\n";

  return 0;
}
