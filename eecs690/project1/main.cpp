/*
 ============================================================================
 Author        : Sharynne Azhar
 Last modified : 8 February 2017
 Description   : Simulates a train scheduling system using threads
 To Compile    : clang++ -std=c++11 -stdlib=libc++ main.cpp (MacOS Sierra)
 ============================================================================
*/

#include <iostream>
#include <fstream>
#include <vector>
#include <stdlib.h>

// Multi threaded libraries
#include <thread>
#include <mutex>

std::mutex coutMutex;
bool go = false;

class Train {
private:
  int m_trainId;
  std::vector<int> m_route;

public:
  Train(int trainId, std::vector<int> route) {
    m_trainId = trainId;
    m_route = route;
  }

  int getId() { return m_trainId; }
  std::vector<int> getRoute() { return m_route; }
};

void travel(Train* train) {
  // wait until all threads are ready
  while (!go) {};

  std::unique_lock<std::mutex> ml(coutMutex);
	std::cout << "I am a train: " << train->getId() << "\n";
	ml.unlock();

  for (int i = 0 ; i < train->getRoute().size(); i++) {
    // do stuff
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
  std::vector<int> routeList;

  for (int i = 0; i < numTrains; i++) {
    file >> numStops;
    for (int j = 0; j < numStops; j++) {
      file >> stop;
      routeList.push_back(stop);
    }
    trains[i] = new std::thread(travel, new Train(i, routeList));
  }

  // run simulation
  go = true;

  coutMutex.lock();
	std::cout << "Hello from the parent thread.\n";
	coutMutex.unlock();

  for (int i = 0 ; i < numTrains ; i++) {
    trains[i]->join();
  }

	std::cout << "Simulation complete.\n";



  return 0;
}
