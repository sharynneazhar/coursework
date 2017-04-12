all: simulator.c
	gcc simulator.c -o simulator

first: all
	./simulator AddressTrace_FirstIndex.bin

last: all
	./simulator AddressTrace_LastIndex.bin

random: all
	./simulator AddressTrace_RandomIndex.bin

test: all
	./simulator AddressTrace_FirstIndex.bin
	./simulator AddressTrace_LastIndex.bin
	./simulator AddressTrace_RandomIndex.bin

clean:
	rm -f *~ simulator
