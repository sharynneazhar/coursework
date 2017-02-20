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

// mutex to prevent output messages from being garbled
std::mutex coutMtx;

// mutex array to store track mutex instances
std::mutex** trackMtxs;

// the barrier makes sure all threads are synchronized in each timestep
Barrier theBarrier;

// ready flag to make sure no thread actually starts processing until all
// n threads have been created
bool ready = false;

// the value to keep track of how many trains are done
int numTrainsDone = 0;

// track i-j is the same as track j-i; returns true if track is lockable
bool lockTrack(int stationI, int stationJ) {
  return trackMtxs[stationI][stationJ].try_lock()
    && trackMtxs[stationJ][stationI].try_lock();
}

// unlocks the tracks that were in use
void unlockTrack(int stationI, int stationJ) {
  trackMtxs[stationI][stationJ].unlock();
  trackMtxs[stationJ][stationI].unlock();
}

void runTrain(Train* train, int numTrains) {

  // wait until all trains threads are initialized
  while (!ready) {};

  // the timestep for each train should be synchronized
  int timeStep = 0;

  // keep going while other trains are still running
  while (numTrainsDone != numTrains) {

    // get the train current stop and next stop
    int currentStop = train->getCurrentStop();
    int nextStop = train->getNextStop();

    if (!train->isAtEnd()) {

      // try to lock the track and advance if unoccupied
      if (lockTrack(currentStop, nextStop)) {
        train->move(timeStep);

        if (train->isAtEnd()) {
          numTrainsDone++;
        }

      } else {
        train->stay(timeStep);
      }

    }

    // hit barrier to wait for the rest of the threads
    theBarrier.barrier(numTrains);

    // unlock the track that was in use
    unlockTrack(currentStop, nextStop);

    // increment to next time step
    timeStep++;

  }

  theBarrier.barrier(numTrains);
}

int main(int argc, char* argv[]) {

  // get information about the trains from file
  std::ifstream file;
  file.open((argc == 2) ? argv[1] : "routes.txt");

  // get total number of trains and stations
  int numTrains, numStations;
  file >> numTrains >> numStations;

  // initialize all the trains
  Train** trains = new Train*[numTrains];

  for (int i = 0; i < numTrains; i++) {

    // get number of stops
    int numStops;
    file >> numStops;

    // get the route
    int* route = new int[numStops];
    for (int j = 0; j < numStops; j++) {
      file >> route[j];
    }

    trains[i] = new Train(i, numStops, route);
  }

  // close file to be safe
  file.close();

  // initialize tracks
  trackMtxs = new std::mutex*[numStations];
  for (int i = 0; i <= numStations; i++) {
    for (int j = 0; j <= numStations; j++) {
      trackMtxs[i] = new std::mutex();
    }
  }

  // create a thread for each train and launch threads
  std::thread** trainThreads = new std::thread*[numTrains];
  for (int i = 0; i < numTrains; i++) {
    trainThreads[i] = new std::thread(runTrain, trains[i], numTrains);
  }

  // run once all threads are ready
  std::cout << "\nStarting simulation...\n\n";
  ready = true;

  // join all threads
  for (int i = 0; i < numTrains; i++) {
    trainThreads[i]->join();
  }

  // All threads are done
  std::cout << "\n\nSimulation complete.\n";

  return 0;
}
