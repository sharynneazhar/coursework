// ContourGenerator.c++ - Code to read a scalar data field and produce
// contours at requested levels.

#include "ContourGenerator.h"

/**************************************************************************
 * OpenCL Startup Variables
 **************************************************************************/
// 1) Platforms
cl_uint numPlatforms = 0;
cl_platform_id* platforms = nullptr;
cl_platform_id curPlatform;
// 2) Devices
cl_uint numDevices = 0;
cl_device_id* devices = nullptr;

/**************************************************************************
 * OpenCL Utility Functions
 **************************************************************************/
const char* readSource(const char* clFileName);

bool debug = false;
void checkStatus(std::string where, cl_int status, bool abortOnError) {
	if (debug || (status != 0)) {
		std::cout << "Step " << where << ", status = " << status << '\n';
	}

	if ((status != 0) && abortOnError) {
		exit(1);
	}
}

void lookAtDeviceLimits(cl_device_id dev) {
	cl_ulong gms;
	cl_ulong lms;
	cl_uint maxCUs;
	size_t mwgs;

	clGetDeviceInfo(dev, CL_DEVICE_GLOBAL_MEM_SIZE, sizeof(cl_ulong), &gms, nullptr);
	clGetDeviceInfo(dev, CL_DEVICE_LOCAL_MEM_SIZE, sizeof(cl_ulong), &lms, nullptr);
	clGetDeviceInfo(dev, CL_DEVICE_MAX_WORK_GROUP_SIZE, sizeof(size_t), &mwgs, nullptr);
	clGetDeviceInfo(dev, CL_DEVICE_MAX_COMPUTE_UNITS, sizeof(cl_uint), &maxCUs, nullptr);

	std::cout << "Device global mem size:     " << gms << '\n';
	std::cout << "Device local mem size:      " << lms << '\n';
	std::cout << "Device max work group size: " << mwgs << '\n';
	std::cout << "Device max compute units:   " << maxCUs << '\n';
	std::cout << '\n';
}

void lookAtKernelLimits(cl_kernel kernel, cl_device_id dev) {
	cl_ulong lms = -1;
	cl_ulong pms = -1;
	size_t warpSize = -1;
	size_t maxWorkGroupSize = -1;

	clGetKernelWorkGroupInfo(kernel, dev, CL_KERNEL_LOCAL_MEM_SIZE, sizeof(cl_ulong), &lms, nullptr);
	clGetKernelWorkGroupInfo(kernel, dev, CL_KERNEL_PRIVATE_MEM_SIZE, sizeof(cl_ulong), &pms, nullptr);
	clGetKernelWorkGroupInfo(kernel, dev, CL_KERNEL_PREFERRED_WORK_GROUP_SIZE_MULTIPLE, sizeof(size_t), &warpSize, nullptr);
	clGetKernelWorkGroupInfo(kernel, dev, CL_KERNEL_WORK_GROUP_SIZE, sizeof(size_t), &maxWorkGroupSize, nullptr);

	std::cout << "Kernel local memory size:   " << lms << '\n';
	std::cout << "Kernel private memory size: " << pms << '\n';
	std::cout << "Kernel warpSize:            " << warpSize << '\n';
	std::cout << "Kernel max work group size: " << maxWorkGroupSize << '\n';
	std::cout << '\n';
}

void reportVersion(cl_platform_id platform) {
	// Get the version of OpenCL supported on this platform
	size_t strLength;
	clGetPlatformInfo(platform, CL_PLATFORM_VERSION, 0, nullptr, &strLength);
	char* version = new char[strLength+1];
	clGetPlatformInfo(platform, CL_PLATFORM_VERSION, strLength+1, version, &strLength);
	std::cout << version << '\n';
	delete [] version;
}

void showProgramBuildLog(cl_program pgm, cl_device_id dev) {
	size_t size;
	clGetProgramBuildInfo(pgm, dev, CL_PROGRAM_BUILD_LOG, 0, nullptr, &size);
	char* log = new char[size+1];
	clGetProgramBuildInfo(pgm, dev, CL_PROGRAM_BUILD_LOG, size+1, log, nullptr);
	std::cout << "LOG:\n" << log << "\n\n";
	delete [] log;
}

/**************************************************************************
 * OpenCL Startup Functions
 **************************************************************************/
/* Returns device index to use or -1 if no available devices found */
int typicalOpenCLProlog(cl_device_type desiredDeviceType) {
	// Discover and initialize the platforms
	cl_int status = clGetPlatformIDs(0, nullptr, &numPlatforms);
	checkStatus("clGetPlatformIDs-0", status, true);
	if (numPlatforms <= 0) {
		std::cout << "No platforms!\n";
		return -1;
	}

	platforms = new cl_platform_id[numPlatforms];
	status = clGetPlatformIDs(numPlatforms, platforms, nullptr);
	checkStatus("clGetPlatformIDs-1", status, true);
	curPlatform = platforms[0];

	if (numPlatforms > 1) {
		size_t platformNameSize = 0;
		clGetPlatformInfo(curPlatform, CL_PLATFORM_NAME, 0, nullptr, &platformNameSize);
		char* name = new char[platformNameSize+1];
		clGetPlatformInfo(curPlatform, CL_PLATFORM_NAME, platformNameSize+1, name, nullptr);
		std::cout << "Found " << numPlatforms << " platforms. Arbitrarily using: " << name << '\n';
		delete [] name;
	}

	reportVersion(curPlatform);

	// Discover and initialize the devices on a specific platform
	status = clGetDeviceIDs(curPlatform, desiredDeviceType, 0, nullptr, &numDevices);
	checkStatus("clGetDeviceIDs-0", status, true);
	if (numDevices <= 0) {
		std::cout << "No devices on platform!\n";
		return -1;
	}

	devices = new cl_device_id[numDevices];
	status = clGetDeviceIDs(curPlatform, desiredDeviceType, numDevices, devices, nullptr);
	checkStatus("clGetDeviceIDs-1", status, true);

	int devIndex = 0;
	if (numDevices > 1) {
		size_t nameLength;
		for (int idx = 0 ; idx < numDevices ; idx++){
			clGetDeviceInfo(devices[idx], CL_DEVICE_NAME, 0, nullptr, &nameLength);
			char* name = new char[nameLength+1];
			clGetDeviceInfo(devices[idx], CL_DEVICE_NAME, nameLength+1, name, nullptr);
			std::cout << "Device " << idx << ": " << name << '\n';
		}

		devIndex = -1;

		while ((devIndex < 0) || (devIndex >= numDevices)) {
			std::cout << "Which device do you want to use? ";
			std::cin >> devIndex;
		}
	} else if (numDevices <= 0) {
		std::cout << "No devices found\n";
	} else {
		std::cout << "Only one device detected\n";
	}

	return devIndex;
}

/**************************************************************************
 * Contour Generator Class Functions
 **************************************************************************/
ContourGenerator::ContourGenerator(std::istream& inp) :
	vertexValues(nullptr)
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

int ContourGenerator::computeContourEdgesFor(float level, vec2*& lines)
{
	// Fire a kernel to determine expected number of edges at the given "level'
	int numExpectedEdges = 2;

	// Create space for the line end points on the device
	int numExpectedPoints = 2 * numExpectedEdges; // each edge is: (x,y), (x,y)

	// Fire a kernel to compute the edge end points (determimes "numActualEdges")
	int numActualEdges = 2;
	int numActualPoints = 2 * numActualEdges; // each edge is: (x,y), (x,y)

	// Get the point coords back, storing them into "lines"
	lines = new vec2[numActualPoints];
	// Use CUDA or OpenCL code to retrieve the points, placing them into "lines".

	int numDimsToUse = 1;

	typicalOpenCLProlog(CL_DEVICE_TYPE_DEFAULT);

	//-------------------------------------------------------------------
	// Create a context for all or some of the discovered devices
	//         (Here we are including all devices.)
	//-------------------------------------------------------------------

	cl_int status;
	cl_context context = clCreateContext(nullptr, numDevices, devices,
		nullptr, nullptr, &status);
	checkStatus("clCreateContext", status, true);

	//-------------------------------------------------------------
	// Create a command queue for ONE device in the context
	//         (There is one queue per device per context.)
	//-------------------------------------------------------------

	cl_command_queue cmdQueue = clCreateCommandQueue(context, devices[0],
		0, &status);
	checkStatus("clCreateCommandQueue", status, true);

	//-----------------------------------------------------
	// Create, compile, and link the program
	//-----------------------------------------------------

	const char* programSource[] = { readSource("Contour.cl") };
	cl_program program = clCreateProgramWithSource(context,
		1, programSource, nullptr, &status);
	checkStatus("clCreateProgramWithSource", status, true);

	status = clBuildProgram(program, numDevices, devices,
		nullptr, nullptr, nullptr);
	checkStatus("clBuildProgram", status, true);

	//-----------------------------------------------------------
	// Create a kernel from one of the __kernel functions
	//         in the source that was built.
	//-----------------------------------------------------------

	cl_kernel kernel = clCreateKernel(program, "contour", &status);

	//-----------------------------------------------------
	// Configure the work-item structure
	//-----------------------------------------------------

	size_t globalWorkSize[] = { 64, 32, 32 };
	size_t localWorkSize[] = { 8, 8, 4 };
	if (numDimsToUse == 1)
		localWorkSize[0] = 32;
	else if (numDimsToUse == 2)
		localWorkSize[0] = localWorkSize[1] = 16;

	//-----------------------------------------------------
	// Enqueue the kernel for execution
	//-----------------------------------------------------

	status = clEnqueueNDRangeKernel(cmdQueue,
		kernel, numDimsToUse, nullptr, globalWorkSize,
		localWorkSize, 0, nullptr, nullptr);
	checkStatus("clEnqueueNDRangeKernel", status, true);

	// block until all commands have finished execution
	clFinish(cmdQueue);

	//-----------------------------------------------------
	// Release OpenCL resources
	//-----------------------------------------------------

	// Free OpenCL resources
	clReleaseKernel(kernel);
	clReleaseProgram(program);
	clReleaseCommandQueue(cmdQueue);
	clReleaseContext(context);

	// Free host resources
	delete [] platforms;
	delete [] devices;

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
	// ... do it here ...

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
