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
priqueue_t q;								// priority queue used to prioritize the jobs
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
  @param running_time the total number of time units this job will run before
				 it will be finished.
  @param priority the priority of the job. (The lower the value, the higher
	       the priority.)
  @return index of core job should be scheduled on
  @return -1 if no scheduling changes should be made.

 */
int scheduler_new_job(int job_number, int time, int running_time, int priority)
{
				// create a new job
				job_t *newJob = malloc(sizeof(job_t));
				newJob->pid = job_number;
				newJob->priority = priority;
				newJob->arrivalTime = time;
				newJob->burstTime = running_time;
				newJob->remainingTime = running_time;
				newJob->responseTime = -1;

				// if scheduler is a single core
				// else we have a multicore scheduler
				if (numCores == 1)
				{
								if (mScheme == FCFS || mScheme == SJF || mScheme == PRI || mScheme == RR)
								{
												if (coreArr[0] == NULL)
												{
																coreArr[0] = newJob;
																coreArr[0]->responseTime = time - coreArr[0]->arrivalTime;
																coreArr[0]->lastScheduledTime = time;
																return 0;
												}

												priqueue_offer(&q, newJob);
												return -1;
								}
								else if (mScheme == PPRI)
								{
												if (coreArr[0] == NULL)
												{
																coreArr[0] = newJob;
																coreArr[0]->responseTime = time - coreArr[0]->arrivalTime;
																coreArr[0]->lastScheduledTime = time;
																return 0;
												}

												int currPriority = coreArr[0]->priority;
												int currArrivalTime = coreArr[0]->arrivalTime;

												if (priority < currPriority || (priority == currPriority && time < currArrivalTime))
												{
																if (coreArr[0]->lastScheduledTime == -1)
																{
																				coreArr[0]->responseTime = -1;
																}

																priqueue_offer(&q, coreArr[0]);
																coreArr[0] = newJob;
																coreArr[0]->responseTime = time - coreArr[0]->arrivalTime;
																coreArr[0]->lastScheduledTime = time;
																return 0;
												}

												priqueue_offer(&q, newJob);
												return -1;
								}
								else
								{
												if (coreArr[0] == NULL)
												{
																coreArr[0] = newJob;
																coreArr[0]->responseTime = time - coreArr[0]->arrivalTime;
																coreArr[0]->lastScheduledTime = time;
																return 0;
												}

												coreArr[0]->remainingTime -= time - coreArr[0]->lastScheduledTime;

												if (coreArr[0]->remainingTime > running_time)
												{
																if (coreArr[0]->responseTime == time - coreArr[0]->arrivalTime)
																{
																				coreArr[0]->responseTime = -1;
																}

																priqueue_offer(&q, coreArr[0]);
																coreArr[0] = newJob;
																coreArr[0]->responseTime = time - coreArr[0]->arrivalTime;
																coreArr[0]->lastScheduledTime = time;
																return 0;
												}

												priqueue_offer(&q, newJob);
												return -1;
								}

				}
				else
				{
								// find an available core
								int openCoreIdx = -1;
								for (int i = 0; i < numCores; i++)
								{
												// found an idle core, break out of loop
												if (coreArr[i] == NULL)
												{
																openCoreIdx = i;
																break;
												}
								}

								// if we have an available core, add the new job
								// else we have to schedule the job
								if (openCoreIdx != -1)
								{
												coreArr[openCoreIdx] = newJob;
												coreArr[openCoreIdx]->responseTime = time - coreArr[openCoreIdx]->arrivalTime;

												if (mScheme == PSJF)
												{
																coreArr[openCoreIdx]->lastScheduledTime = time;
												}

												return openCoreIdx;
								}

								if (mScheme == FCFS || mScheme == SJF || mScheme == PRI || mScheme == RR)
								{
												priqueue_offer(&q, newJob);
												return -1;
								}
								else if (mScheme == PPRI)
								{
												int lowestPriority = coreArr[0]->priority;
												int lowestIndex = 0;

												for (int i = 0; i < numCores; i++)
												{
																if (coreArr[i]->priority > lowestPriority)
																{
																				lowestPriority = coreArr[i]->priority;
																				lowestIndex = i;
																}
																else if (coreArr[i]->priority == lowestPriority)
																{
																				if (coreArr[i]->arrivalTime > coreArr[lowestIndex]->arrivalTime)
																				{
																								lowestIndex = i;
																				}
																}
												}

												if (lowestPriority > priority)
												{
																if (coreArr[lowestIndex]->lastScheduledTime == time)
																{
																				coreArr[lowestIndex]->responseTime = -1;
																}

																priqueue_offer(&q, coreArr[lowestIndex]);
																coreArr[lowestIndex] = newJob;
																coreArr[lowestIndex]->responseTime = time - coreArr[lowestIndex]->arrivalTime;
																return lowestIndex;
												}
												else if (lowestPriority == priority)
												{
																for (int i = lowestIndex; i < numCores; i++)
																{
																				if (coreArr[i]->arrivalTime > coreArr[lowestIndex]->arrivalTime)
																				{
																								lowestIndex = i;
																				}
																}

																if (coreArr[lowestIndex]->arrivalTime > time)
																{
																				if (coreArr[lowestIndex]->lastScheduledTime == time)
																				{
																								coreArr[lowestIndex]->responseTime = -1;
																				}

																				priqueue_offer(&q, coreArr[lowestIndex]);
																				coreArr[lowestIndex] = newJob;
																				coreArr[lowestIndex]->responseTime = time - coreArr[lowestIndex]->arrivalTime;
																				return lowestIndex;
																}

																priqueue_offer(&q, newJob);
																return -1;
												}

												priqueue_offer(&q, newJob);
												return -1;
								}
								else
								{
												coreArr[0]->remainingTime -= time - coreArr[0]->lastScheduledTime;
												coreArr[0]->lastScheduledTime = time;

												int greatestRemainingTime = coreArr[0]->remainingTime;
												int highestIndex = 0;

												// find the shortest remainingTime
												for (int i = 0; i < numCores; i++)
												{
																// calculate new remainingTime
																coreArr[i]->remainingTime -= time - coreArr[i]->lastScheduledTime;
																coreArr[i]->lastScheduledTime = time;

																// see if current remainingTime is lower than greatestRemainingTime
																if (coreArr[i]->remainingTime > greatestRemainingTime)
																{
																					highestIndex = i;
																					greatestRemainingTime = coreArr[i]->remainingTime;
																}
												}

												if (greatestRemainingTime > running_time)
												{
																if (coreArr[highestIndex]->responseTime == (time - coreArr[highestIndex]->arrivalTime))
																{
																				coreArr[highestIndex]->responseTime = -1;
																}

																priqueue_offer(&q, coreArr[highestIndex]);
																coreArr[highestIndex] = newJob;

																if (coreArr[highestIndex]->responseTime == -1)
																{
																				coreArr[highestIndex]->responseTime = time - coreArr[highestIndex]->arrivalTime;
																}

																return highestIndex;
												}

												priqueue_offer(&q, newJob);
												return -1;
								}
				}

				return -1;
}

/**
  Called when a job has completed execution.

  The core_id, job_number and time parameters are provided for convenience.
	You may be able to calculate the values with your own data structure.
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
					totalResponseTime += coreArr[core_id]->responseTime;
					totalWaitingTime += time - coreArr[core_id]->arrivalTime - coreArr[core_id]->burstTime;
					totalTurnAroundTime += time - coreArr[core_id]->arrivalTime;
					numJobs++;

					free(coreArr[core_id]);
					coreArr[core_id] = NULL;

					if (priqueue_size(&q) > 0)
					{
									job_t *nextJob = (job_t*)priqueue_poll(&q);
									coreArr[core_id] = nextJob;

									if (coreArr[core_id]->responseTime == -1)
									{
													coreArr[core_id]->lastScheduledTime = time;
													coreArr[core_id]->responseTime = time - coreArr[core_id]->arrivalTime;
									}

									if (mScheme == PSJF)
									{
													nextJob->lastScheduledTime = time;
													if (coreArr[core_id]->responseTime == -1)
													{
																	coreArr[core_id]->responseTime = time - coreArr[core_id]->arrivalTime;
													}
									}

									return coreArr[core_id]->pid;
					}

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
				// find job in array of cores
				job_t *thisJob = coreArr[core_id];

				// if null then the job isn't running
				// else put the job back in queue
				if (thisJob == NULL)
				{
								if (priqueue_size(&q) == 0)
								{
												return -1;
								}
				}
				else
				{
								priqueue_offer(&q, thisJob);
				}

				// run the next job in the queue
				coreArr[core_id] = priqueue_poll(&q);
				if (coreArr[core_id]->responseTime == -1)
				{
								int resTime = time - coreArr[core_id]->arrivalTime;
								coreArr[core_id]->responseTime = resTime;
				}

				return coreArr[core_id]->pid;
}


/**
  Returns the average waiting time of all jobs scheduled by your scheduler.

  Assumptions:
    - This function will only be called after all scheduling is complete (all j
		obs that have arrived will have finished and no new jobs will arrive).

  @return the average waiting time of all jobs scheduled.
 */
float scheduler_average_waiting_time()
{
				return ((float) totalWaitingTime / (float) numJobs);
}


/**
  Returns the average turnaround time of all jobs scheduled by your scheduler.

  Assumptions:
    - This function will only be called after all scheduling is complete (all
		jobs that have arrived will have finished and no new jobs will arrive).

  @return the average turnaround time of all jobs scheduled.
 */
float scheduler_average_turnaround_time()
{
				return ((float) totalTurnAroundTime / (float) numJobs);
}


/**
  Returns the average response time of all jobs scheduled by your scheduler.

  Assumptions:
    - This function will only be called after all scheduling is complete (all
		jobs that have arrived will have finished and no new jobs will arrive).

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
  In our provided output, we have implemented this function to list the jobs
	in the order they are to be scheduled. Furthermore, we have also listed the
	current state of the job (either running on a given core or idle). For
	example, if we have a non-preemptive algorithm and job(id=4) has began
	running, job(id=2) arrives with a higher priority, and job(id=1) arrives
	with a lower priority, the output in our sample output will be:

    2(-1) 4(0) 1(-1)

  This function is not required and will not be graded. You may leave it
  blank if you do not find it useful.
 */
void scheduler_show_queue()
{

}
