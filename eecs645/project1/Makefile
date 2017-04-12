#######################################################################
# Makefile
# Author : Sharynne Azhar
#######################################################################

all: simulator.c
	gcc simulator.c -o simulator

first: all
	./simulator AddressTraces/AddressTrace_FirstIndex.bin
	@rm simulator

last: all
	./simulator AddressTraces/AddressTrace_LastIndex.bin
	@rm simulator

random: all
	./simulator AddressTraces/AddressTrace_RandomIndex.bin
	@rm simulator

test: all
	@mkdir -p Results
	@echo "Running all address traces... This may take a bit..."
	./simulator AddressTraces/AddressTrace_FirstIndex.bin > Results/FirstIndex.txt
	./simulator AddressTraces/AddressTrace_LastIndex.bin > Results/LastIndex.txt
	./simulator AddressTraces/AddressTrace_RandomIndex.bin > Results/RandomIndex.txt
	@rm simulator
