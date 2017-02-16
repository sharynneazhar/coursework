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

public:
  // Default constructor
  Train(int trainId, int numStops, int* route)
    : m_trainId(trainId), m_numStops(numStops), m_route(route), currentIdx(0) {}

  // converts int to char and returns the character (i.e. 1 == 'A')
  char getId() {
    return m_trainId + 65;
  }

  // returns the number of stops on route
  // (minus 1 because we want to stay at the last stop)
  int getNumStops() {
    return m_numStops - 1;
  }

  // returns the station number
  int getStation(int idx) {
    return m_route[idx];
  }

  // returns the current station number
  int getCurrentStopIdx() {
    return currentIdx;
  }

  // go to next station
  void travel() {
    currentIdx++;
  }

  // return true if the train is at its final destination
  bool isAtEnd() {
    return currentIdx == m_numStops - 1;
  }

};

#endif
