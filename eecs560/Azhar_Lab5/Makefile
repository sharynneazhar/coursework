#######################################################################
# General Makefile for compiling c++ programs
# Author : Sharynne Azhar
#######################################################################

# Specify the lab number
LAB_NUM=5

# Specify the folder name pattern
FOLDER_NAME := Azhar_Lab$(LAB_NUM)

# Specify a name for the executable
PROG_NAME	:= lab$(LAB_NUM)

# Compilation settings
CC := g++ -std=c++11
CFLAGS := -g -Wall

# Specify the files needed for compilation
INCLUDES = $(wildcard ./*.h)
SRC_FILES = $(filter-out ./test.txt, $(wildcard ./*))

# A generalized rule for compiling the executable object
$(PROG_NAME): main.o
	$(CC) $(CFLAGS) main.o -o $(PROG_NAME)

# A generalized rule for compiling c++ source files
main.o: main.cpp $(INCLUDES)
	$(CC) $(CFLAGS) -c main.cpp

# Target for testing the program with a series of inputs
test:
	./$(PROG_NAME) < test.txt

# Target for cleaning the generated files
clean:
	@rm -rf *.o $(PROG_NAME)

# Target for compressing the files into archive
zip: clean
	zip -r $(FOLDER_NAME).zip $(SRC_FILES)


.PHONY: clean zip
