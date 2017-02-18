/*
 ============================================================================
 Author        : Sharynne Azhar
 Date          : 8 February 2017
 Description   : Simulates a train scheduling system using threads
 To Compile    : make; make run
 ============================================================================
*/

#include <fstream>
#include <iostream>
#include <mutex>
#include <thread>

#include "Barrier.h"
#include "Train.h"

Barrier theBarrier;
std::mutex coutMtx;
std::mutex** trackMtxs;
bool ready = false;

bool lockTrack(int stationI, int stationJ) {
  return trackMtxs[stationI][stationJ].try_lock()
    && trackMtxs[stationJ][stationI].try_lock();
}

void unlockTrack(int stationI, int stationJ) {
  trackMtxs[stationI][stationJ].unlock();
  trackMtxs[stationJ][stationI].unlock();
}

void runTrain(Train* train, int numTrains) {
  int timeStep = 0;

  // wait until all trains threads are initialized
  while (!ready) {};

  // make sure the timeStep keeps going even when the train
  // is done to satisfy barrier condition
  while (timeStep != 12) {
    int currentStop = train->getCurrentStop();
    int nextStop = train->getNextStop();

    if (!train->isAtEnd()) {
      // if track is clear (mutex lockable) advance, else stay
      coutMtx.lock();
      if (lockTrack(currentStop, nextStop)) {
        train->move(timeStep);
      } else {
        train->stay(timeStep);
      }
      coutMtx.unlock();
    }

    theBarrier.barrier(numTrains);
    unlockTrack(currentStop, nextStop);
    timeStep++;
  }
}

int main(int argc, char* argv[]) {

  // get information about the number of trains and stations
  int numTrains, numStations, numStops;

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

  // initialize tracks - create a mutex instance for
  // all possible tracks
  trackMtxs = new std::mutex*[numStations];
  for (int i = 0; i <= numStations; i++) {
    for (int j = 0; j <= numStations; j++) {
      trackMtxs[i] = new std::mutex();
    }
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
