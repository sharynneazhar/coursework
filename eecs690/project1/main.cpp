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
#include <stdlib.h>

#include <thread>
#include <mutex>

#include "Barrier.h"

Barrier barrier;
std::mutex TRAIN_MUTEX;
bool GO = false;

class Train {
private:
  int m_trainId;
  std::queue<int> m_route;

public:
  // Default constructor
  Train(int trainId, std::queue<int> route)
    : m_trainId(trainId), m_route(route) {}

  // converts int to char and returns the character (i.e. 1 == 'A')
  char getId() { return m_trainId + 65; }

  // returns the train route
  std::queue<int>& getRoute() { return m_route; }

  // return true if the train route is done (i.e. queue empty)
  bool isAtEnd() { return m_route.empty(); }
};

void travel(Train* train, int numTrains) {
  // wait until all trains threads are ready
  while (!GO) {};

  while (!train->isAtEnd()) {
    std::unique_lock<std::mutex> ml(TRAIN_MUTEX);
    std::cout << "I am train " << train->getId() << " ";
    std::cout << "going from " << train->getRoute().front() << " ";
    train->getRoute().pop();
    std::cout << "to " << train->getRoute().front() << std::endl;
  }
}

int main(int argc, char* argv[]) {

  if (argc != 2) {
    std::cerr << "\nMissing file.\n";
    exit(EXIT_FAILURE);
  }

  // get information about the number of trains and stations
  int numTrains, numStations;
  std::ifstream file;
  file.open(argv[1]);
  file >> numTrains >> numStations;

  // create a thread for each train
  std::thread** trains = new std::thread*[numTrains];

  // get routes for each train
  int numStops, stop;
  for (int i = 0; i < numTrains; i++) {
    file >> numStops;
    std::queue<int> routeList;
    for (int j = 0; j < numStops; j++) {
      file >> stop;
      routeList.push(stop);
    }
    trains[i] = new std::thread(travel, new Train(i, routeList), numTrains);
  }

  std::cout << "\nStarting simulation...\n\n";

  // threads are ready run simulation
  GO = true;

  for (int i = 0; i < numTrains; i++) {
    trains[i]->join();
  }

  // All threads are done
  std::cout << "\n\nSimulation complete.\n";

  return 0;
}
