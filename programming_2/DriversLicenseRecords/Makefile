lab02: main.o Executive.o DriversLicenseRecord.o ExecutiveReaderException.o
	g++ -std=c++11 -g -Wall main.o Executive.o DriversLicenseRecord.o ExecutiveReaderException.o -o lab02

main.o: main.cpp
	g++ -std=c++11 -g -Wall -c main.cpp

Executive.o: Executive.h Executive.cpp
	g++ -std=c++11 -g -Wall -c Executive.cpp

DriversLicenseRecord.o: DriversLicenseRecord.h DriversLicenseRecord.cpp
	g++ -std=c++11 -g -Wall -c DriversLicenseRecord.cpp

ExecutiveReaderException.o: ExecutiveReaderException.h ExecutiveReaderException.cpp
	g++ -std=c++11 -g -Wall -c ExecutiveReaderException.cpp

clean:
	rm *.o lab02
	echo clean done
