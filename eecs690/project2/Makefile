proj2:
	mpic++ main.cpp -o proj2

test: clean
	mpic++ main.cpp -o proj2
	mpirun -np 20 ./proj2 sr max D
	mpirun -np 20 ./proj2 sr min D
	mpirun -np 20 ./proj2 sr avg CO
	mpirun -np 20 ./proj2 sr number AS gt 55
	mpirun -np 4 ./proj2 bg max D E I M
	mpirun -np 10 ./proj2 bg max D E I M AS BQ CO DK AA Y

clean:
	@rm -rf *.o proj2
