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

class Train {
private:
  int m_trainId;
  int m_numStops;
  int* m_route;
  int currentIdx;
  std::mutex mtx;

  // converts int to char and returns the character (i.e. 1 == 'A')
  char getId() { return m_trainId + 65; }

public:
  
  // Default constructor
  Train(int trainId, int numStops, int* route)
    : m_trainId(trainId), m_numStops(numStops), m_route(route), currentIdx(0) {}

  // return true if the train is at its final destination
  bool isAtEnd() { return currentIdx == m_numStops - 1; }

  // returns the number of stops on route
  // (minus 1 because we want to stay at the last stop)
  int getNumStops() { return m_numStops - 1; }

  // returns the current stop id
  int getCurrentStop() { return m_route[currentIdx]; }

  // returns the next stop id
  int getNextStop() { return m_route[currentIdx + 1]; }

  // go to next stop
  void travel() {
    mtx.lock();
    std::cout << "Train " << getId() << " ";
    std::cout << "going from station " << m_route[currentIdx] << " ";
    std::cout << "to station " << m_route[currentIdx + 1] << std::endl;
    currentIdx++;
    mtx.unlock();
  }

  // stay at current stop
  void stay() {
    mtx.lock();
    std::cout << "Train " << getId() << " ";
    std::cout << "must stay at station " << m_route[currentIdx] << ".\n";
    mtx.lock();
  }

};

#endif
