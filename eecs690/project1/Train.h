/*
 ============================================================================
 Author        : Sharynne Azhar
 Date          : 8 February 2017
 Description   : Handles train methods
 ============================================================================
*/

#ifndef TRAIN_H
#define TRAIN_H

#include <iostream>
std::mutex mtx;

class Train {
private:
  int m_trainId;
  int m_numStops;
  int* m_route;
  int currentIdx;

  // converts int to char and returns the character (i.e. 1 == 'A')
  char getId() { return m_trainId + 65; }

public:

  // Default constructor
  Train(int trainId, int numStops, int* route)
    : m_trainId(trainId), m_numStops(numStops), m_route(route), currentIdx(0) {}

  // return true if the train is at its final destination
  bool isAtEnd() { return currentIdx == m_numStops - 1; }

  // returns the number of stops on route
  int getNumStops() { return m_numStops; }

  // returns the current stop id
  int getCurrentStop() { return m_route[currentIdx]; }

  // returns the next stop id
  int getNextStop() { return m_route[currentIdx + 1]; }

  // go to next stop
  void move(int timeStep) {
    std::lock_guard<std::mutex> guard(mtx);
    std::cout << "At time step " << timeStep << ": ";
    std::cout << "Train " << getId() << " ";
    std::cout << "going from station " << m_route[currentIdx] << " ";
    std::cout << "to station " << m_route[currentIdx + 1] << std::endl;
    currentIdx++;
  }

  // stay at current stop
  void stay(int timeStep) {
    std::lock_guard<std::mutex> guard(mtx);
    std::cout << "At time step " << timeStep << ": ";
    std::cout << "Train " << getId() << " ";
    std::cout << "must stay at station " << m_route[currentIdx] << ".\n";
  }

};

#endif
