#######################################################################
# General Makefile for compiling c programs
#
# Author : EECS-678 Staff
#######################################################################

# Specify the name of the program to build
PROG_NAME	:= lab1

# Specify CFLAGS for compilation
CFLAGS		:= -Wall

# Specify linker flags
LDFLAGS		:= -lm

# Specify all the source files requried for this program
SRC		:= $(patsubst %.c, %.o, $(wildcard *.c))

# Default make target
all: execute

# Target for executing the program
execute: $(PROG_NAME)
	./$<

# A generalized rule for linking the compiled files into executable object
$(PROG_NAME): $(SRC)
	gcc $^ $(LDFLAGS) -o $@

# A generalized rule for compiling c source files
%.o: %.c
	gcc $(CFLAGS) -c $<

# Target for cleaning the generated files
clean:
	rm -f *.o *~ $(PROG_NAME)

# Target for compressing the files into archive
tar: 
	mkdir editors
	cp dot_vimrc Makefile simple.* editors/
	tar cvzf eecs678-editors-lab.tar.gz editors
	rm -rf editors
