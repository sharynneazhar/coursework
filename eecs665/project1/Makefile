#######################################################################
# General Makefile for compiling c++ programs
# Author : Sharynne Azhar
#######################################################################

# Specify the lab number
LAB_NUM=

# Specify the folder name pattern
FOLDER_NAME := Azhar_Project1

# Specify a name for the executable
PROG_NAME	:= nfa2dfa

# Compilation settings
CC := g++ -std=c++11
CFLAGS := -g -Wall

# Specify the files needed for compilation
SRC = ./
INCLUDES =

all: clean $(PROG_NAME)

test: clean $(PROG_NAME)
	./$(PROG_NAME) < input.txt

# A generalized rule for compiling the executable object
$(PROG_NAME): main.o
	$(CC) $(CFLAGS) main.o -o $(PROG_NAME)

# A generalized rule for compiling c++ source files
main.o: main.cpp $(INCLUDES)
	$(CC) $(CFLAGS) -c main.cpp

# Target for cleaning the generated files
clean:
	@rm -rf *.o $(PROG_NAME)

# Target for compressing the files into archive
tar: clean
	tar -cvzf $(FOLDER_NAME).tar.gz $(SRC)


.PHONY: clean zip
