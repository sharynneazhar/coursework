#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>

using namespace std;

class Timer {
private:
	timeval startTime;
  timeval endTime;

public:

	void start(){
		gettimeofday(&startTime, NULL);
	}

	double stop() {
	  long seconds, nSeconds;
		double duration;

		gettimeofday(&endTime, NULL);

		seconds  = endTime.tv_sec  - startTime.tv_sec;
		nSeconds = endTime.tv_usec - startTime.tv_usec;

		duration = seconds + nSeconds / 1000000.0;

		return duration;
	}

	void printTime(double duration){
		printf("%5.6f seconds\n", duration);
	}

};
