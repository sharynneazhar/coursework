// ContourGenerator.c++ - Code to read a scalar data field and produce
// contours at requested levels.

#include "ContourGenerator.h"

/**************************************************************************
 * Contour Generator Class Functions
 **************************************************************************/
ContourGenerator::ContourGenerator(std::istream& inp) : vertexValues(nullptr)
{
	inp >> nRowsOfVertices >> nColsOfVertices;
	std::string scalarDataFieldFileName;
	inp >> scalarDataFieldFileName;
	std::ifstream scalarFieldFile(scalarDataFieldFileName.c_str());
	if (scalarFieldFile.good())
	{
		readData(scalarFieldFile);
		scalarFieldFile.close();
	}
	else
	{
		std::cerr << "Could not open " << scalarDataFieldFileName
		          << " for reading.\n";
		nRowsOfVertices = nColsOfVertices = 0;
	}
}

ContourGenerator::~ContourGenerator()
{
	if (vertexValues != nullptr)
		delete [] vertexValues;
	// Delete any GPU structures (buffers) associated with this model as well!
}

int ContourGenerator::fireNumExpectedEdgesKernel(int nRows, int nCols, float level) {
	int count = 0;
	size_t datasize = nRows * nCols * sizeof(float);

	cl_int status;

	// Create device buffers associated with the context
	cl_mem vertexBuf = clCreateBuffer(context, CL_MEM_READ_ONLY,
		datasize, nullptr, &status);
	checkStatus("clCreateBuffer-A", status, true);

	cl_mem intBuf = clCreateBuffer(context, CL_MEM_WRITE_ONLY,
		sizeof(int), nullptr, &status);
	checkStatus("clCreateBuffer-B", status, true);

	// Enqueue buffers ready to write
	status = clEnqueueWriteBuffer(cmdQueue, vertexBuf, CL_FALSE, 0, datasize,
		vertexValues, 0, nullptr, nullptr);
	checkStatus("clEnqueueWriteBuffer-A", status, true);

	status = clEnqueueWriteBuffer(cmdQueue, intBuf, CL_FALSE, 0, sizeof(int),
		&count, 0, nullptr, nullptr);
	checkStatus("clEnqueueWriteBuffer-B", status, true);

	// Set kernel arguments
	status = clSetKernelArg(kernels[0], 0, sizeof(cl_mem), &vertexBuf);
	checkStatus("clSetKernelArg-A", status, true);
	status = clSetKernelArg(kernels[0], 1, sizeof(cl_mem), &intBuf);
	checkStatus("clSetKernelArg-B", status, true);
	status = clSetKernelArg(kernels[0], 2, sizeof(int), &nRowsOfVertices);
	checkStatus("clSetKernelArg-C", status, true);
	status = clSetKernelArg(kernels[0], 3, sizeof(int), &nColsOfVertices);
	checkStatus("clSetKernelArg-D", status, true);
	status = clSetKernelArg(kernels[0], 4, sizeof(float), &level);
	checkStatus("clSetKernelArg-E", status, true);

	// Set work-item structure
	size_t localWorkSize[] = { 16, 16 };
	size_t globalWorkSize[2];

	for (int d = 0; d < 2; d++) {
		globalWorkSize[d] = nRows * nCols;
		if (globalWorkSize[d] % localWorkSize[d] != 0) {
			globalWorkSize[d] = (((nRows * nCols) / localWorkSize[d]) + 1) * localWorkSize[d];
		}
	}

	// Enqueue kernel for execution
	status = clEnqueueNDRangeKernel(cmdQueue, kernels[0], 1, nullptr,
		globalWorkSize, localWorkSize, 0, nullptr, nullptr);
	checkStatus("clEnqueueNDRangeKernel-A", status, true);

	// Read back to host
	clEnqueueReadBuffer(cmdQueue, intBuf, CL_TRUE, 0, sizeof(int),
		&count, 0, nullptr, nullptr);

	// Block until all commands are finished
	clFinish(cmdQueue);

	// Free buffers
	clReleaseMemObject(vertexBuf);
	clReleaseMemObject(intBuf);

	return count;
}

int ContourGenerator::computeContourEdgesFor(float level, vec2*& lines) {
	// Initialize and build OpenCL
	cl_int status;
	const char* kNames[] = { "computeNumExpectedEdges", "computeEdges" };

	initializeOpenCL();
	buildProgram("Contour.cl", kNames, 2);

	// Fire a kernel to compute the number of expected edges
	int numExpectedEdges = fireNumExpectedEdgesKernel(nRowsOfVertices,
		nColsOfVertices, level);

	printf("\n>> NUM EXPECTED EDGES = %d", numExpectedEdges);

	// Create space for the line end points on the device
	int numExpectedPoints = 2 * numExpectedEdges; // each edge is: (x,y), (x,y)

	// Fire a kernel to compute the edge end points (determimes "numActualEdges")
	int numActualEdges = 0;
	int count = 0;
	int loc = 0;

	size_t datasize = nRowsOfVertices * nColsOfVertices * sizeof(float);
	size_t lineBufSize = numExpectedPoints * 2 * sizeof(float);

	// Create device buffers associated with the context
	cl_mem vertexBuf = clCreateBuffer(context, CL_MEM_READ_ONLY,
		datasize, nullptr, &status);
	checkStatus("clCreateBuffer-A", status, true);

	cl_mem lineBuf = clCreateBuffer(context, CL_MEM_WRITE_ONLY,
		lineBufSize, nullptr, &status);
  checkStatus("clCreateBuffer-B", status, true);

  cl_mem endPointCount = clCreateBuffer(context, CL_MEM_READ_WRITE,
		sizeof(int), nullptr, &status);
  checkStatus("clCreateBuffer-C", status, true);

  cl_mem loc_index = clCreateBuffer(context, CL_MEM_READ_WRITE,
		sizeof(int), nullptr, &status);
  checkStatus("clCreateBuffer-D", status, true);

	// Enqueue buffers ready to write
	status = clEnqueueWriteBuffer(cmdQueue, endPointCount, CL_FALSE, 0,
		sizeof(int), &numActualEdges, 0, nullptr, nullptr);
	checkStatus("clEnqueueWriteBuffer-E", status, true);

	status = clEnqueueWriteBuffer(cmdQueue, loc_index, CL_FALSE, 0,
		sizeof(int), &loc, 0, nullptr, nullptr);
	checkStatus("clEnqueueWriteBuffer-F", status, true);

	// Set kernel arguments
	status = clSetKernelArg(kernels[1], 0, sizeof(cl_mem), &vertexBuf);
	checkStatus("clSetKernelArg-A", status, true);
	status = clSetKernelArg(kernels[1], 1, sizeof(cl_mem), &lineBuf);
	checkStatus("clSetKernelArg-B", status, true);
  status = clSetKernelArg(kernels[1], 2, sizeof(cl_mem), &endPointCount);
	checkStatus("clSetKernelArg-C", status, true);
  status = clSetKernelArg(kernels[1], 3, sizeof(cl_mem), &loc_index);
	checkStatus("clSetKernelArg-D", status, true);
	status = clSetKernelArg(kernels[1], 4, sizeof(int), &nRowsOfVertices);
	checkStatus("clSetKernelArg-E", status, true);
	status = clSetKernelArg(kernels[1], 5, sizeof(int), &nColsOfVertices);
	checkStatus("clSetKernelArg-F", status, true);
  status = clSetKernelArg(kernels[1], 6, sizeof(float), &level);
	checkStatus("clSetKernelArg-G", status, true);

	// Set work-item structure
	size_t localWorkSize[] = { 16, 16 };
	size_t globalWorkSize[2];

	for (int d = 0; d < 2; d++) {
		globalWorkSize[d] = nRowsOfVertices * nColsOfVertices;
		if (globalWorkSize[d] % localWorkSize[d] != 0) {
			globalWorkSize[d] = (((nRowsOfVertices * nColsOfVertices) /
				localWorkSize[d]) + 1) * localWorkSize[d];
		}
	}

	// Enqueue kernel for execution
	status = clEnqueueNDRangeKernel(cmdQueue, kernels[1], 1, nullptr,
		globalWorkSize, localWorkSize, 0, nullptr, nullptr);
	checkStatus("clEnqueueNDRangeKernel", status, true);

	// Read back to host
	clEnqueueReadBuffer(cmdQueue, endPointCount, CL_TRUE, 0, sizeof(int),
		&numActualEdges, 0, nullptr, nullptr);

	// Block until all commands are finished
	clFinish(cmdQueue);

	printf("\n>> NUM ACTUAL EDGES = %d\n\n", numActualEdges);

	int numActualPoints = 2 * numActualEdges; // each edge is: (x,y), (x,y)
	float* tmpLines = new float[numActualPoints * 2];
	lines = new vec2[numActualPoints];

	clEnqueueReadBuffer(cmdQueue, lineBuf, CL_TRUE, 0,
		(numActualPoints * 2 * sizeof(float)), tmpLines, 0, nullptr, nullptr);

	for (int i = 0; i < numActualPoints; i++){
    lines[i][0] = tmpLines[2 * i];
    lines[i][1] = tmpLines[2 * i + 1];
  }

  delete [] tmpLines;

	// After the line end points have been returned from the device, delete the
	// device buffer to prevent a memory leak.
	releaseOpenCLResources();

	// Free buffers
	clReleaseMemObject(vertexBuf);
	clReleaseMemObject(lineBuf);
	clReleaseMemObject(endPointCount);

	// return number of coordinate pairs in "lines":
	return numActualPoints;
}

void ContourGenerator::readData(std::ifstream& scalarFieldFile)
{
	vertexValues = new float[nRowsOfVertices * nColsOfVertices];
	int numRead = 0, numMissing = 0;
	float val;
	float minVal = 1.0, maxVal = -1.0;
	scalarFieldFile.read(reinterpret_cast<char*>(&val),sizeof(float));
	while (!scalarFieldFile.eof())
	{
		vertexValues[numRead++] = val;
		if (val == -9999)
			numMissing++;
		else if (minVal > maxVal)
			minVal = maxVal = val;
		else
		{
			if (val < minVal)
				minVal = val;
			else if (val > maxVal)
				maxVal = val;
		}
		scalarFieldFile.read(reinterpret_cast<char*>(&val),sizeof(float));
	}
	std::cout << "read " << numRead << " values; numMissing: " << numMissing
	          << "; range of values: " << minVal << " <= val <= " << maxVal << '\n';
}
