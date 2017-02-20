/*
 ============================================================================
 Author        : James Miller
 Description   : Best implementation of a barrier using condition_variables
 Usage:        : 1. Create an instance of a Barrier class (called, say, "b")
                    that is accessible to, but outside the scope of any thread
                    code that needs to use it.
                 2. In the thread code where barrier synchronization is to
                    occur, each thread in the "barrier group" must execute:
                    b.barrier(num); // where "num" is the number of threads in
                    the "barrier group"
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
		std::unique_lock<std::mutex> barrierLock(barrierMutex);
		barrierCounter++;
		if (barrierCounter != numExpectedAtBarrier) {
      std::cout << "count: " << barrierCounter << "expected: " << numExpectedAtBarrier << std::endl;
			barrierCV.wait(barrierLock);
    } else {
			barrierCounter = 0;
			barrierCV.notify_all();
		}
	}
};

#endif
