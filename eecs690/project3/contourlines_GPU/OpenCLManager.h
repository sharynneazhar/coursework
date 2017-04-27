// OpenCLManager.h - OpenCL utilities

#ifndef OPENCL_MANAGER_H
#define OPENCL_MANAGER_H

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
 * OpenCL Startup Variables
 **************************************************************************/
// 1) Platforms
extern cl_uint numPlatforms;
extern cl_platform_id* platforms;
extern cl_platform_id curPlatform;

// 2) Devices
extern cl_uint numDevices;
extern cl_device_id* devices;

// 3) context and queue
extern cl_context context;
extern cl_command_queue cmdQueue;

// 4) kernels
extern cl_kernel* kernels;
extern int numKernels;
extern cl_program program;

/**************************************************************************
 * OpenCL Utility Functions
 **************************************************************************/
void checkStatus(std::string where, cl_int status, bool abortOnError);
void reportVersion(cl_platform_id platform);
void lookAtDeviceLimits(cl_device_id dev);
void lookAtDeviceLimits(cl_device_id dev);
void showProgramBuildLog(cl_program pgm, cl_device_id dev);

/**************************************************************************
 * OpenCL Startup Functions
 **************************************************************************/
void typicalOpenCLProlog(cl_device_type desiredDeviceType);
void initializeOpenCL();
void buildProgram(const char* src, const char* kernelName[], int nKernels);
void releaseOpenCLResources();

#endif
