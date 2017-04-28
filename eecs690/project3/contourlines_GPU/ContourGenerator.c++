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

int ContourGenerator::fireNumExpectedEdgesKernel(int numVertices, size_t datasize, float level) {
	int count = 0;
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

	for (int d = 0 ; d < 2 ; d++) {
		globalWorkSize[d] = numVertices;
		if (globalWorkSize[d] % localWorkSize[d] != 0) {
			globalWorkSize[d] = ((numVertices / localWorkSize[d]) + 1) * localWorkSize[d];
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

	int numVertices = nRowsOfVertices * nColsOfVertices;
	size_t datasize = numVertices * sizeof(float);

	printf("\n>> DATA SIZE = %d", datasize);

	int numExpectedEdges = fireNumExpectedEdgesKernel(numVertices, datasize, level);

	printf("\n>> NUM EXPECTED EDGES = %d\n\n", numExpectedEdges);

	// Create space for the line end points on the device
	int numExpectedPoints = 2 * numExpectedEdges; // each edge is: (x,y), (x,y)

	// Fire a kernel to compute the edge end points (determimes "numActualEdges")
	int numActualEdges = 2;
	int numActualPoints = 2 * numActualEdges; // each edge is: (x,y), (x,y)

	// Get the point coords back, storing them into "lines"
	lines = new vec2[numActualPoints];
	// Use CUDA or OpenCL code to retrieve the points, placing them into "lines".
	// As a placeholder for now, we will just make an "X" over the area:
	lines[0][0] = 0.0;
	lines[0][1] = 0.0;

	lines[1][0] = nColsOfVertices - 1.0;
	lines[1][1] = nRowsOfVertices - 1.0;

	lines[2][0] = 0.0;
	lines[2][1] = nRowsOfVertices - 1.0;

	lines[3][0] = nColsOfVertices - 1.0;
	lines[3][1] = 0.0;

	// After the line end points have been returned from the device, delete the
	// device buffer to prevent a memory leak.
	releaseOpenCLResources();

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
