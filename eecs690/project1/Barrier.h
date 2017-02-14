/*
 ============================================================================
 Author        : James Miller
 Description   : "Best" implementation of a barrier using condition_variables
 ============================================================================
*/

#ifndef BARRIER_H
#define BARRIER_H

#include <mutex>
#include <condition_variable>

class Barrier {
private:
  int barrierCounter;
  std::mutex barrierMutex;
  std::condition_variable barrierCV;

public:
	Barrier() : barrierCounter(0) {}
	virtual ~Barrier() {}

	void barrier(int numExpectedAtBarrier) {
		std::unique_lock<std::mutex> ulbm(barrierMutex);
		barrierCounter++;
		if (barrierCounter != numExpectedAtBarrier) {
			barrierCV.wait(ulbm);
    } else {
			barrierCounter = 0;
			barrierCV.notify_all();
		}
	}
};

#endif
