/** @file libscheduler.c
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "libscheduler.h"
#include "../libpriqueue/libpriqueue.h"


/***************************************************************************
 * Global Declarations
 ***************************************************************************/

/**
  The global priority queue used to prioritize  the jobs
*/
priqueue_t q;

/**
  Stores information making up a job to be scheduled including any statistics.
=*/
typedef struct _job_t
{
				int pid;								// the unique id
				int priority;						// the job priority
				int core;								// the core assigned (zero-indexed), -1 if idle
				int arrivalTime;				// the job arrival time
				int burstTime;					// the total time units before job is finished
				int remainingTime;  		// the total time units remaining
				int waitTime;						// the total time units job has been waiting
				int responseTime;				// the total time units job takes to respond
				int lastScheduledTime;	// the time this job was last scheduled
} job_t;

/**
  Details about the scheduler
*/
scheme_t mScheme;						// the scheme scheduler is running
job_t **coreArr;						// the array of cores
int numCores; 							// the number of cores in use
int numJobs;								// the number of jobs in the scheduler
float totalWaitingTime;			// the total waiting time
float totalTurnAroundTime;	// the total turnAround time
float totalResponseTime;		// the total response time


/***************************************************************************
 * Compare Functions
 ***************************************************************************/

/**
	First come first serve (FCFS) compare function
	Assigns the CPU based on the order of the requests
*/
int FCFSComparer(const void *a, const void *b)
{
				return 1;
}

/**
	Shortest job first (SJF) compare function
	Executes the job with the shortest CPU burst first
*/
int SJFComparer(const void *a, const void *b)
{
				return (((job_t *)a)->burstTime - ((job_t *)b)->burstTime);
}

/**
	Preemptive SJF (PSJF) compare function
	Also known as shortest time remaining first (SRTF). If a job has been
	partially executed, it schedules a job based on its remaining time (not
	the full remaining time)
*/
int PSJFComparer(const void *a, const void *b)
{
				return (((job_t *)a)->remainingTime - ((job_t *)b)->remainingTime);
}

/**
	Priority (PRI) compare function
*/
int PRIComparer(const void *a, const void *b)
{
				// if the priorities are equal, compare the arrival times
				if (((job_t *)a)->priority == ((job_t *)b)->priority)
				{
								return ((job_t *)a)->arrivalTime - ((job_t *)b)->arrivalTime;
				}

				return ((job_t *)a)->priority - ((job_t *)b)->priority;
}

/**
	Preemptive priority (PPRI) compare function
*/
int PPRIComparer(const void *a, const void *b)
{
				// if the priorities are equal, compare the arrival times
				if (((job_t *)a)->priority == ((job_t *)b)->priority)
				{
								return ((job_t *)a)->arrivalTime - ((job_t *)b)->arrivalTime;
				}

				return ((job_t *)a)->priority - ((job_t *)b)->priority;
}

/**
	Round robin (RR) compare function
	Basically a preemptive FCFS. When a new job arrives, it must be placed at
	the end of the cycle of jobs
*/
int RRComparer(const void *a, const void *b)
{
				return 1;
}


/***************************************************************************
 * Scheduler Functions
 ***************************************************************************/

/**
  Initalizes the scheduler.

  Assumptions:
    - You may assume this will be the first scheduler function called.
    - You may assume this function will be called once once.
    - You may assume that cores is a positive, non-zero number.
    - You may assume that scheme is a valid scheduling scheme.

  @param cores the number of cores that is available by the scheduler. These
	       cores will be known as core(id=0), core(id=1), ..., core(id=cores-1).
  @param scheme  the scheduling scheme that should be used. This value will be
	       one of the six enum values of scheme_t
*/
void scheduler_start_up(int cores, scheme_t scheme)
{
				numCores = cores;
				coreArr = malloc(cores * sizeof(job_t));
				for (int i = 0; i < cores; i++)
				{
								coreArr[i] = NULL;
				}

				mScheme = scheme;
				numJobs = 0;
				totalWaitingTime = 0.0;
				totalResponseTime = 0.0;
				totalTurnAroundTime = 0.0;

				switch (scheme)
				{
								case FCFS:
												priqueue_init(&q, FCFSComparer);
												break;
								case SJF:
												priqueue_init(&q, SJFComparer);
												break;
								case PSJF:
												priqueue_init(&q, PSJFComparer);
												break;
								case PRI:
												priqueue_init(&q, PRIComparer);
												break;
								case PPRI:
												priqueue_init(&q, PPRIComparer);
												break;
								case RR:
												priqueue_init(&q, RRComparer);
												break;
				}
}


/**
  Called when a new job arrives.

  If multiple cores are idle, the job should be assigned to the core with the
  lowest id.
  If the job arriving should be scheduled to run during the next
  time cycle, return the zero-based index of the core the job should be
  scheduled on. If another job is already running on the core specified,
  this will preempt the currently running job.
  Assumptions:
    - You may assume that every job wil have a unique arrival time.

  @param job_number a globally unique identification number of the job arriving.
  @param time the current time of the simulator.
  @param running_time the total number of time units this job will run before it will be finished.
  @param priority the priority of the job. (The lower the value, the higher the priority.)
  @return index of core job should be scheduled on
  @return -1 if no scheduling changes should be made.

 */
int scheduler_new_job(int job_number, int time, int running_time, int priority)
{
	return -1;
}


/**
  Called when a job has completed execution.

  The core_id, job_number and time parameters are provided for convenience. You may be able to calculate the values with your own data structure.
  If any job should be scheduled to run on the core free'd up by the
  finished job, return the job_number of the job that should be scheduled to
  run on core core_id.

  @param core_id the zero-based index of the core where the job was located.
  @param job_number a globally unique identification number of the job.
  @param time the current time of the simulator.
  @return job_number of the job that should be scheduled to run on core core_id
  @return -1 if core should remain idle.
 */
int scheduler_job_finished(int core_id, int job_number, int time)
{
	return -1;
}


/**
  When the scheme is set to RR, called when the quantum timer has expired
  on a core.

  If any job should be scheduled to run on the core free'd up by
  the quantum expiration, return the job_number of the job that should be
  scheduled to run on core core_id.

  @param core_id the zero-based index of the core where the quantum has expired.
  @param time the current time of the simulator.
  @return job_number of the job that should be scheduled on core cord_id
  @return -1 if core should remain idle
 */
int scheduler_quantum_expired(int core_id, int time)
{
	return -1;
}


/**
  Returns the average waiting time of all jobs scheduled by your scheduler.

  Assumptions:
    - This function will only be called after all scheduling is complete (all jobs that have arrived will have finished and no new jobs will arrive).

  @return the average waiting time of all jobs scheduled.
 */
float scheduler_average_waiting_time()
{
				return ((float) totalWaitingTime / (float) numJobs);
}


/**
  Returns the average turnaround time of all jobs scheduled by your scheduler.

  Assumptions:
    - This function will only be called after all scheduling is complete (all jobs that have arrived will have finished and no new jobs will arrive).

  @return the average turnaround time of all jobs scheduled.
 */
float scheduler_average_turnaround_time()
{
				return ((float) totalTurnAroundTime / (float) numJobs);
}


/**
  Returns the average response time of all jobs scheduled by your scheduler.

  Assumptions:
    - This function will only be called after all scheduling is complete (all jobs that have arrived will have finished and no new jobs will arrive).

  @return the average response time of all jobs scheduled.
 */
float scheduler_average_response_time()
{
				return ((float) totalResponseTime / (float) numJobs);
}


/**
  Free any memory associated with your scheduler.

  Assumptions:
    - This function will be the last function called in your library.
*/
void scheduler_clean_up()
{
				priqueue_destroy(&q);

				for (int i = 0; i < numCores; i++)
				{
								if (coreArr[i] != NULL)
								{
												free(coreArr[i]);
								}
				}

				free(coreArr);
}


/**
  This function may print out any debugging information you choose. This
  function will be called by the simulator after every call the simulator
  makes to your scheduler.
  In our provided output, we have implemented this function to list the jobs in the order they are to be scheduled. Furthermore, we have also listed the current state of the job (either running on a given core or idle). For example, if we have a non-preemptive algorithm and job(id=4) has began running, job(id=2) arrives with a higher priority, and job(id=1) arrives with a lower priority, the output in our sample output will be:

    2(-1) 4(0) 1(-1)

  This function is not required and will not be graded. You may leave it
  blank if you do not find it useful.
 */
void scheduler_show_queue()
{

}
