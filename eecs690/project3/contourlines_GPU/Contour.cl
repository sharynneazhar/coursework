#pragma OPENCL EXTENSION cl_khr_fp64 : enable

__kernel
void hello(void) {
	// printf("\nHello OpenCL!\n");
}

__kernel
void computeNumExpectedEdges(__global float* vertexBuf, __global int* numEdges, int nRows, int nCols, float level) {
	int numVertices = nRows * nCols;
	int block = get_global_id(0);
	size_t gridSize = get_global_size(0);

	int numAbove = 0;
	int numBelow = 0;

	for(int i = block*2; i < (numVertices*2); i += gridSize){
    if(vertexBuf[i] > level){
      numAbove++;
    }
    else numBelow++;
    if(vertexBuf[i+1] > level){
      numAbove++;
    }
    else numBelow++;
    if(vertexBuf[i+(nCols*2)] > level){
      numAbove++;
    }
    else numBelow++;
    if(vertexBuf[i+(nCols*2)+1] > level){
      numAbove++;
    }
    else numBelow++;

    if(   (numAbove == 1 && numBelow == 3) ||
          (numAbove == 3 && numBelow == 1)  ){

      atomic_inc(numEdges);
    }

    if(   (numAbove == 2 && numBelow == 2) ){
      atomic_inc(numEdges);
      atomic_inc(numEdges);
    }

  }
}

__kernel
void computeEdges(__global float* linesBuf) {
	linesBuf[0] = 33;
}
