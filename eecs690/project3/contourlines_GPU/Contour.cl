#pragma OPENCL EXTENSION cl_khr_fp64 : enable

__kernel
void computeNumExpectedEdges(__global float* vertexBuf, __global int* numEdges,
	int nRows, int nCols, float level) {

	int block = get_global_id(0);
	int numBlocks = (nRows - 1) * (nCols - 1);

	size_t gridSize = get_global_size(0);

	for (int i = block; i < numBlocks; i+= gridSize) {
		int r = (int) (i / (nCols - 1));
		int c = i % (nCols - 1);

		float tl = vertexBuf[(r + 1) * nCols  + c];
		float tr = vertexBuf[(r + 1) * nCols  + c + 1];
		float bl = vertexBuf[r * nCols + c];
    float br = vertexBuf[r * nCols + c + 1];

		// Handle missing data
		if (bl != -9999 && br != -9999 && tl != -9999 && tr != -9999) {
			// Top left
	    if ((bl > level && br > level && tl < level && tr > level) ||
	        (bl < level && br < level && tl > level && tr < level)) {
	      atomic_inc(numEdges);
	    }

	    // Top Right
	    if ((bl > level && br > level && tl > level && tr < level) ||
	        (bl < level && br < level && tl < level && tr > level)) {
	      atomic_inc(numEdges);
	    }

			// Bottom left
	    if ((bl < level && br > level && tl > level && tr > level) ||
	        (bl > level && br < level && tl < level && tr < level)) {
	      atomic_inc(numEdges);
	    }

	    // Bottom Right
	    if ((bl > level && br < level && tl > level && tr > level) ||
	        (bl < level && br > level && tl < level && tr < level)) {
	      atomic_inc(numEdges);
	    }

	    // Left and Right
	    if ((bl < level && br > level && tl < level && tr > level) ||
	        (bl > level && br < level && tl > level && tr < level)) {
	      atomic_inc(numEdges);
	    }

	    // Top and Bottom
	    if ((bl > level && br > level && tl < level && tr < level) ||
	        ( bl < level && br < level && tl > level && tr > level)) {
	      atomic_inc(numEdges);
	    }

	    // Corner
	    if ((bl > level && br < level && tl < level && tr > level) ||
	        (bl < level && br > level && tl > level && tr < level)) {
	      atomic_add(numEdges, 2);
	    }
    }
	}
}

__kernel
void computeEdges(__global float* vertexBuf, __global float* lineBuf,
	__global int* numPoints, __global int* loc, int nRows, int nCols, float level) {

  int block = get_global_id(0);
  int numBlocks = (nRows - 1) * (nCols - 1);

  size_t gridSize = get_global_size(0);

	for (int i = block; i < numBlocks; i += gridSize){
		int r = (int) (i / (nCols - 1));
		int c = i % (nCols - 1);

		float tl = vertexBuf[(r + 1) * nCols  + c];
		float tr = vertexBuf[(r + 1) * nCols  + c + 1];
		float bl = vertexBuf[r * nCols + c];
    float br = vertexBuf[r * nCols + c + 1];

    if (bl != -9999 && br != -9999 && tl != -9999 && tr != -9999) {
      // Bottom left
      if ((bl < level && br > level && tl > level && tr > level) ||
          (bl > level && br < level && tl < level && tr < level)) {


      }

      // Bottom right
      if ((bl > level && br < level && tl > level && tr > level ) ||
          (bl < level && br > level && tl < level && tr < level)) {

      }
    }
  }
}
