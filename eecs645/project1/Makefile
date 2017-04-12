#######################################################################
# Makefile
# Author : Sharynne Azhar
#######################################################################

PROG := ./simulator

all: simulator.c
	gcc simulator.c -o simulator

first: all
	$(PROG) AddressTraces/AddressTrace_FirstIndex.bin

last: all
	$(PROG) AddressTraces/AddressTrace_LastIndex.bin

random: all
	$(PROG) AddressTraces/AddressTrace_RandomIndex.bin

test: all
	@mkdir -p Results
	@echo "Running all address traces... This may take a bit..."
	$(PROG) AddressTraces/AddressTrace_FirstIndex.bin > Results/FirstIndex.txt
	$(PROG) AddressTraces/AddressTrace_LastIndex.bin > Results/LastIndex.txt
	$(PROG) AddressTraces/AddressTrace_RandomIndex.bin > Results/RandomIndex.txt

clean:
	rm -f *~ simulator
