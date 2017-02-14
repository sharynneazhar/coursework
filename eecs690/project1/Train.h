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
  std::queue<int> m_route;

public:
  // Default constructor
  Train(int trainId, std::queue<int> route)
    : m_trainId(trainId), m_route(route) {}

  // converts int to char and returns the character (i.e. 1 == 'A')
  char getId() {
    return m_trainId + 65;
  }

  // returns the train route
  std::queue<int>& getRoute() {
    return m_route;
  }

  void goToNextStop() {
    m_route.pop();
  }

  // return true if the train route is done (i.e. queue empty)
  bool isAtEnd() {
    return m_route.size() == 1;
  }
};

#endif
