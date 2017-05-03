#######################################################################
# Makefile
# Author : Sharynne Azhar
#######################################################################

PROGNAME = ALUSimulator

# Compilation settings
CC = gcc --std=gnu11
CFLAGS = -g

# Add files to appropriate line to get this makefile to build them.
CFILELIST = RegisterFile_01.c ALUSimulator.c ALUSimulator_Main.c
HFILELIST =  RegisterFile_01.h ALUSimulator.h

# Include locations
INCLIST = ./src

# Add libraries that need linked as needed
LIBLIST =

# Project source files directory
SRCDIR = ./src/

# Object files directory
OBJDIR = ./obj/

CFILES = $(patsubst %,$(SRCDIR)%,$(CFILELIST))
HFILES = $(patsubst %,$(SRCDIR)%,$(HFILELIST))
OFILES = $(patsubst %.c,$(OBJDIR)%.o,$(CFILELIST))

RAWC = $(patsubst %.c,%,$(addprefix $(SRCDIR), $(CFILELIST)))
RAWH = $(patsubst %.h,%,$(addprefix $(SRCDIR), $(HFILELIST)))

INCDIRS = $(patsubst %,-I%,$(INCLIST))
OBJINNERDIRS = $(patsubst $(SRCDIR)%,$(OBJDIR)%,$(shell find $(SRCDIR) -type d))

# Build the executable
all: $(OBJINNERDIRS) $(PROGNAME)

# Build the object directories
$(OBJINNERDIRS):
	$(foreach dir, $(OBJINNERDIRS), mkdir -p $(dir);)

# Build the program
$(PROGNAME): $(OFILES)
	$(CC) $(CFLAGS) $^ -o $(PROGNAME) $(LIBLIST)

# Generic build target for all compilation units.
$(OBJDIR)%.o: $(SRCDIR)%.c $(HFILES)
	$(CC) $(CFLAGS) -c $(INCDIRS) -o $@ $<

# Build and run the program
test: all
	./$(PROGNAME) > stdout.txt
	diff -wB stdout.txt stdexp.txt
	make clean

# Remove all generated files and directories
clean:
	@rm -rf $(PROGNAME) obj stdout.txt

.PHONY: clean
