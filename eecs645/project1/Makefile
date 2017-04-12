#######################################################################
# Makefile
# Author : Sharynne Azhar
#######################################################################

all:
	gcc simulator_1way.c -o simulator_1way
	gcc simulator_4way.c -o simulator_4way

first: all
	./simulator_1way AddressTraces/AddressTrace_FirstIndex.bin
	./simulator_4way AddressTraces/AddressTrace_FirstIndex.bin
	@rm simulator_1way simulator_4way

last: all
	./simulator_1way AddressTraces/AddressTrace_LastIndex.bin
	./simulator_4way AddressTraces/AddressTrace_LastIndex.bin
	@rm simulator_1way simulator_4way

random: all
	./simulator_1way AddressTraces/AddressTrace_RandomIndex.bin
	./simulator_4way AddressTraces/AddressTrace_LastIndex.bin
	@rm simulator_1way simulator_4way

test: all
	@mkdir -p Results
	@echo "Running all address traces... This may take a bit..."
	./simulator_1way AddressTraces/AddressTrace_FirstIndex.bin > Results/FirstIndex_1way.txt
	./simulator_1way AddressTraces/AddressTrace_LastIndex.bin > Results/LastIndex_1way.txt
	./simulator_1way AddressTraces/AddressTrace_RandomIndex.bin > Results/RandomIndex_1way.txt
	./simulator_4way AddressTraces/AddressTrace_FirstIndex.bin > Results/FirstIndex_4way.txt
	./simulator_4way AddressTraces/AddressTrace_LastIndex.bin > Results/LastIndex_4way.txt
	./simulator_4way AddressTraces/AddressTrace_RandomIndex.bin > Results/RandomIndex_4way.txt
	@rm simulator_1way simulator_4way
