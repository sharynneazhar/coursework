all: simulator.c
	gcc simulator.c -o simulator

first: all
	./simulator AddressTrace_FirstIndex.bin

last: all
	./simulator AddressTrace_LastIndex.bin

random: all
	./simulator AddressTrace_RandomIndex.bin

test: all first last random

clean:
	rm -f *~ simulator
