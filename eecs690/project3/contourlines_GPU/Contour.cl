#pragma OPENCL EXTENSION cl_khr_fp64 : enable

__kernel
void hello(void) {
	// printf("\nHello OpenCL!\n");
}

__kernel
void computeNumExpectedEdges() {
	int globalID = get_global_id(0);
	int localID = get_local_id(0);
	int gridSize = get_global_size(0);
	int localSize = get_local_size(0);

	// printf("global: (%d), local: (%d), gridSize: (%d), localSize: (%d)\n",
	// 	globalID, localID, gridSize, localSize);
}

__kernel
void contour(void) {
	int globalID[3], localID[3];
	int gridLoc[3]; // "grid" as in CUDA's grid loc
	for (int i=0 ; i<3 ; i++) {
		globalID[i] = get_global_id(i);
		localID[i] = get_local_id(i);
		gridLoc[i] = globalID[i] / get_local_size(i);
	}

	printf("global: (%d, %d, %d), gridLoc: (%d, %d, %d), local: (%d, %d, %d)\n",
		globalID[0], globalID[1], globalID[2],
		gridLoc[0], gridLoc[1], gridLoc[2],
		localID[0], localID[1], localID[2]);
}
