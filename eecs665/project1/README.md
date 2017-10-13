# Project 1 - NFA to DFA

### Description

Your assignment is to implement the subset construction algorithm (Figure 3.32 in the text) to convert an NFA to the equivalent DFA. You will also need to implement other algorithms, such as those for the move (Figure 3.31) and –closure (Figure 3.33) operations, required for subset construction.

Both the input NFA and the output DFA will be represented as a transition table. You should represent the NFA and DFA states as integer numbers, starting at 1. A special symbol ’E’ will be used to indicate the –transition.

The input will come from standard input and the output should be printed to standard output (monitor). You should attempt to match your output as closely to my output as possible. This assignment should be implemented in either C or C++. Provide a Makefile that will include commands to build your program.

The main goal of this project is to practice implementing a moderately complex algorithm in a systems programming language. This assignment will also test your ability to read formatted input and generate formatted output.

You should not make any assumptions regarding the maximum number of states in the input NFA or the output DFA. You may assume that: (a) there can be a maximum of twenty-six input symbols (‘a’ to ‘z’), (b) the &epsilon;–transition will always be the last column in the input.


### Running the Program
