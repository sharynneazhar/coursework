In this lab, you will do some time performance measuring on three hashing methods
you programmed on Lab2 and Lab3. Choose the size of the hash table as 600011
(prime number), start the timer and generate random numbers between 1 to
2,147,483,647 (long) for load factor 0.2, 0.3, 0.4…0.9; save the time for each
load factor. Let K be 60000 (The number of times trying to find a place to
insert an element). There should be no duplication in the hash table, so be
careful to first search for the number and if not in the table then add it.
rand() and srand() and Timer class will be discussed in the lab.

Calculate the total time for the different load factors and different probing
method (open hashing, quadratic and double hashing). Apply for 5 different
seeds and calculate the average. Mention the time for 5 different seeds in
your report, and draw a chart for the average time (time versus load factor).
To ensure that the timing tests are “fair,” you must use the same seed for the
random number generator for each structure. Submit a report to discuss:

(1) The overall organization of your experiment,

(2) Data generation,

(3) Summary of results (CPU timing)

(4) Observation and conclusion.
