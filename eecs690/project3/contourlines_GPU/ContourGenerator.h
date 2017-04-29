// ContourGenerator.h - Code to read a scalar data field and produce
// contours at requested levels.

#ifndef CONTOURGENERATOR_H
#define CONTOURGENERATOR_H

/**************************************************************************
 * System Includes
 **************************************************************************/
#include <iostream>
#include <fstream>
#include <math.h>
#include <string>
#include <string.h>

/**************************************************************************
 * OpenCL Includes
 **************************************************************************/
#ifdef __APPLE__
	#include <OpenCL/opencl.h>
#else
	#include <CL/opencl.h>
#endif

#include "OpenCLManager.h"

/**************************************************************************
 * Public Types and Global Definitions
 **************************************************************************/
typedef float vec2[2];

/**************************************************************************
 * Contour Generator Class
 **************************************************************************/
class ContourGenerator
{
public:
	ContourGenerator(std::istream& inp);
	virtual ~ContourGenerator();

	int fireNumExpectedEdgesKernel(int nRows, int nCols, float level);
	// int fireComputeEdgesEndPointsKernel(int nRows, int nCols, float level);

  // Fires a GPU kernel to compute edges for the given level and return them
  int computeContourEdgesFor(float level, vec2*& lines);

	size_t getNumberCols() const { return nColsOfVertices; }
	size_t getNumberRows() const { return nRowsOfVertices; }

private:
	void readData(std::ifstream& scalarFieldFile);

	size_t nRowsOfVertices, nColsOfVertices;
	float* vertexValues;
};

#endif
