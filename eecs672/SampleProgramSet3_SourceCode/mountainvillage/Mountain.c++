// Mountain.c++ - concrete subclass of ModelView that represents a Mountain

#include <iostream>
#include <cmath>

#include "Mountain.h"

typedef float vec2[2];

Mountain::Mountain(ShaderIF* sIF, double xMinIn, double xMaxIn, double yMinIn,
		double yMaxIn, double freq, double phase,
		int numSamplePointsIn) : shaderIF(sIF),
	xMin(xMinIn), xMax(xMaxIn), yMin(yMinIn), yMax(yMaxIn),
	frequency(freq), phaseShift(phase),
	numSamplePoints(numSamplePointsIn)
{
	defineModel();
}

Mountain::~Mountain()
{
	glDeleteBuffers(2, vbo);
	glDeleteVertexArrays(1, vao);
}

void Mountain::defineModel()
{
	int numStripPoints = 4 * numSamplePoints;
	vec2 points[numStripPoints];

	double theta = -M_PI;
	double x = xMin;
	double dx = (xMax - xMin) / (numSamplePoints - 1);
	double dTheta = (2 * M_PI) / (numSamplePoints - 1);

	for (int i = 0; i < numSamplePoints; i += 2) {
		points[i][0] = x;
		points[i][1] = cos((frequency * theta) + phaseShift);

		points[i + 1][0] = x + dx;
		points[i + 1][1] = cos((frequency * (theta + dTheta)) + phaseShift);

		x += dx;
		theta += dTheta;
	}

	// send the data to GPU
	glGenVertexArrays(1, vao);
	glBindVertexArray(vao[0]);

	glGenBuffers(1, vbo);
	glBindBuffer(GL_ARRAY_BUFFER, vbo[0]);
	glBufferData(GL_ARRAY_BUFFER, numStripPoints * sizeof(vec2), points, GL_STATIC_DRAW);
	glVertexAttribPointer(shaderIF->pvaLoc("mcPosition"), 2, GL_FLOAT, GL_FALSE, 0, 0);
	glEnableVertexAttribArray(shaderIF->pvaLoc("mcPosition"));
}

// xyzLimits: {mcXmin, mcXmax, mcYmin, mcYmax, mcZmin, mcZmax}
void Mountain::getMCBoundingBox(double* xyzLimits) const
{
	xyzLimits[0] = xMin;
	xyzLimits[1] = xMax;
	xyzLimits[2] = yMin;
	xyzLimits[3] = yMax;
	// All z coordinates are implicitly zero, nevertheless:
	xyzLimits[4] = -1.0;
	xyzLimits[5] = 1.0;
}

void Mountain::render()
{
	// save the current GLSL program in use
	GLint pgm;
	glGetIntegerv(GL_CURRENT_PROGRAM, &pgm);
	// draw the triangles using our vertex and fragment shaders
	glUseProgram(shaderIF->getShaderPgmID());

	// define the mapping from MC to -1..+1 Logical Device Space:
	float scaleTrans[4];
	compute2DScaleTrans(scaleTrans);
	glUniform4fv(shaderIF->ppuLoc("scaleTrans"), 1, scaleTrans);

	glBindVertexArray(vao[0]);
	glDrawArrays(GL_TRIANGLE_STRIP, 0, 2*numSamplePoints);
	// restore the previous program
	glUseProgram(pgm);
}
