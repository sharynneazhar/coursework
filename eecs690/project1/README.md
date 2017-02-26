# Project 1: Train Simulation

Suppose you are responsible for the design of a train scheduling system with the following operational specifications:

1. There are n trains identified by letters: A, B, …. (Assume n ≤ 26).
2. There are m stations identified by sequential numbers: 0, 1, 2, …, m-1.
3. There is at most a single track between any pair of stations, hence only one train at a time may be traveling in either direction along the track between a given pair of stations.
4. All stations have the capacity to hold n trains.
5. A given track will be identifed as i–j: the track between station i and station j. Note that i–j and j–i denote the same track.
6. At the start of a day, each train will be given an assigned route in the form of the list of stations to which it is to travel. For example: 3, 5, 1, 7, 6, 4. You are to assume:
  * Each train is initially at the first station in this list.
  * Whenever the list has: …, i, j, …, assume that i≠j, there is a direct track between i and j, it takes one unit of time to go from i to j along this track, and the train must use that track to go from i to j.
  * At the end of the day, the train will stay at the last station in the list.

Consider a simulation in which we determine at each time step which trains that are not yet at their final destination are cleared to go to the next station in their list (or whether they must stay where they are for that time step).
