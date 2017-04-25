#define COMPILE_PRINTF 1

__kernel
void contour(void)
{
	int globalID[3], localID[3];
	int gridLoc[3]; // "grid" as in CUDA's grid loc
	for (int i=0 ; i<3 ; i++)
	{
		globalID[i] = get_global_id(i);
		localID[i] = get_local_id(i);
		gridLoc[i] = globalID[i] / get_local_size(i);
	}
#if COMPILE_PRINTF
	// only with "real" OpenCL; not via CUDA. (And not on NVIDIA, even with OpenCL?)
	printf("global: (%d, %d, %d), gridLoc: (%d, %d, %d), local: (%d, %d, %d)\n",
		globalID[0], globalID[1], globalID[2],
		gridLoc[0], gridLoc[1], gridLoc[2],
		localID[0], localID[1], localID[2]);
#endif
}
