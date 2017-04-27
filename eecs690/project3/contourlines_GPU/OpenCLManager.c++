// OpenCLManager.c++ - OpenCL utilities

/**************************************************************************
 * System Includes
 **************************************************************************/
#include <iostream>
#include <string>

/**************************************************************************
 * OpenCL Includes
 **************************************************************************/
#ifdef __APPLE__
	#include <OpenCL/opencl.h>
#else
	#include <CL/opencl.h>
#endif

/**************************************************************************
 * OpenCL Utility Functions
 **************************************************************************/
const char* readSource(const char* clFileName);

bool debug = false;
void checkStatus(std::string where, cl_int status, bool abortOnError) {
	if (debug || (status != 0))
		std::cout << "Step " << where << ", status = " << status << '\n';
	if ((status != 0) && abortOnError)
		exit(1);
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
 // 1) Platforms
cl_uint numPlatforms = 0;
cl_platform_id* platforms = nullptr;
cl_platform_id curPlatform;

// 2) Devices
cl_uint numDevices = 0;
cl_device_id* devices = nullptr;

// 3) context and queue
cl_context context;
cl_command_queue cmdQueue;

// 4) kernels
int numKernels = 0;
cl_kernel* kernels = nullptr;
cl_program program;

void typicalOpenCLProlog(cl_device_type desiredDeviceType) {
	//-----------------------------------------------------
	// Discover and query the platforms
	//-----------------------------------------------------

	cl_int status = clGetPlatformIDs(0, nullptr, &numPlatforms);
	checkStatus("clGetPlatformIDs-0", status, true);

	platforms = new cl_platform_id[numPlatforms];

	status = clGetPlatformIDs(numPlatforms, platforms, nullptr);
	checkStatus("clGetPlatformIDs-1", status, true);
	curPlatform = platforms[0];

	reportVersion(curPlatform);

	//-------------------------------------------------------------------------------
	// Discover and initialize the devices on the selected (current) platform
	//-------------------------------------------------------------------------------

	status = clGetDeviceIDs(curPlatform, desiredDeviceType, 0, nullptr, &numDevices);
	checkStatus("clGetDeviceIDs-0", status, true);

	devices = new cl_device_id[numDevices];

	status = clGetDeviceIDs(curPlatform, desiredDeviceType, numDevices, devices, nullptr);
	checkStatus("clGetDeviceIDs-1", status, true);
}

void initializeOpenCL() {
	typicalOpenCLProlog(CL_DEVICE_TYPE_DEFAULT);

	cl_int status;
	context = clCreateContext(nullptr, numDevices, devices, nullptr, nullptr, &status);
	checkStatus("clCreateContext", status, true);

	cmdQueue = clCreateCommandQueue(context, devices[0], 0, &status);
	checkStatus("clCreateCommandQueue", status, true);
}

void buildProgram(const char* src, const char* kernelName[], int nKernels) {
  cl_int status;
  const char* programSource[] = { readSource(src) };

	program = clCreateProgramWithSource(context, 1, programSource, nullptr, &status);
	checkStatus("clCreateProgramWithSource", status, true);

	status = clBuildProgram(program, numDevices, devices, nullptr, nullptr, nullptr);
	checkStatus("clBuildProgram", status, false);

  if (status != 0) {
		showProgramBuildLog(program, devices[0]);
		exit(1);
	}

	numKernels = nKernels;
	kernels = new cl_kernel[numKernels];
	for (int i = 0; i < numKernels; i++) {
		kernels[i] = clCreateKernel(program, kernelName[i], &status);
  }
}

void releaseOpenCLResources() {
	// Free OpenCL resources
	for (int i = 0; i < numKernels; i++) {
		clReleaseKernel(kernels[i]);
  }

	clReleaseProgram(program);
	clReleaseCommandQueue(cmdQueue);
	clReleaseContext(context);

	// Free host resources
	delete [] platforms;
	delete [] devices;
	delete [] kernels;
}
