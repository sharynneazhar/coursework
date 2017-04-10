#######################################################################
# General Makefile for compiling c++ programs
# Author : Sharynne Azhar
#######################################################################

# Specify the lab number
LAB_NUM=9

# Specify the folder name pattern
FOLDER_NAME := Azhar_Lab$(LAB_NUM)

# Specify a name for the executable
PROG_NAME	:= lab$(LAB_NUM)

# Compilation settings
CC := g++ -std=c++11
CFLAGS := -g -Wall

# Specify the files needed for compilation
SRC = ../
INCLUDES = ./Queue/Queue.h ./BinomialQueueNode/BinomialQueueNode.h ./BinomialQueue.h

all: clean $(PROG_NAME)

test: all
	g++ Test/test1.cpp -o test1
	g++ Test/test2.cpp -o test2
	g++ Test/test3.cpp -o test3
	./test1 > test1out.txt
	./test2 > test2out.txt
	./test3 > test3out.txt
	diff -wB test1out.txt Test/test1exp.txt
	diff -wB test2out.txt Test/test2exp.txt
	diff -wB test3out.txt Test/test3exp.txt
	@echo "No output is good output."
	@rm -rf test1 test2 test3 *out.txt
	@make clean

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
zip: clean
	zip -r $(FOLDER_NAME).zip $(SRC)


.PHONY: clean zip
