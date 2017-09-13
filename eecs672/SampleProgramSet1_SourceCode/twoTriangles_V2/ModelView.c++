// ModelView.c++ - a basic combined Model and View for OpenGL

#include <iostream>

#include "ModelView.h"

// Current MC Region of interest
double ModelView::mcRegionOfInterest[6] = { -1.0, 1.0, -1.0, 1.0, -1.0, 1.0 };

static const int numVerticesInTriangle = 3; // same as in Hello, OpenGL

ModelView::ModelView(ShaderIF* sIF, vec2* triangleVertices) :
	shaderIF(sIF), colorMode(7)
{
	initModelGeometry(triangleVertices);
}

ModelView::~ModelView()
{
	// delete the vertex array objects and buffers, if they have not
	// already been deleted
	deleteObject();
}

void ModelView::compute2DScaleTrans(float* scaleTransF) // CLASS METHOD
{
	// We are only concerned with the xy extents for now
	// Map the desired limits to the -1..+1 Logical Device Space:
	double scaleTrans[4];
	linearMap(mcRegionOfInterest[0], mcRegionOfInterest[1], -1.0, 1.0,
		scaleTrans[0], scaleTrans[1]);
	linearMap(mcRegionOfInterest[2], mcRegionOfInterest[3], -1.0, 1.0,
		scaleTrans[2], scaleTrans[3]);
	for (int i=0 ; i<4 ; i++)
		scaleTransF[i] = static_cast<float>(scaleTrans[i]);
}

void ModelView::deleteObject()
{
	if (vao[0] > 0) // hasn't already been deleted
	{
		glDeleteBuffers(1, vbo);
		glDeleteVertexArrays(1, vao);
		vao[0] = vbo[0] = 0;
	}
}

// xyzLimits: {mcXmin, mcXmax, mcYmin, mcYmax, mcZmin, mcZmax}
void ModelView::getMCBoundingBox(double* xyzLimits) const
{
	xyzLimits[0] = xmin; xyzLimits[1] = xmax;
	xyzLimits[2] = ymin; xyzLimits[3] = ymax;
	xyzLimits[4] = -1.0; xyzLimits[5] =  1.0; // (zmin, zmax) (really 0..0)
}

bool ModelView::handleCommand(unsigned char anASCIIChar, double ldsX, double ldsY)
{
	// determine if this triangle was "picked" based on whether the
	// given location is inside the bounding box of this triangle.
	float st[4];
	compute2DScaleTrans(st);
	float xMC = (ldsX - st[1]) / st[0];
	float yMC = (ldsY - st[3]) / st[2];
	if ((xMC < xmin) || (xMC > xmax) || (yMC < ymin) || (yMC > ymax))
		// No, this triangle was not picked; tell Controller to keep looking
		// for someone to handle this event.
		return true;

	// Yes - it is this triangle: Now apply the current operation to it:
	if ((anASCIIChar >= '0') && (anASCIIChar <= '9'))
		// set a new color mode to be used in the fragment shader
		colorMode = static_cast<int>(anASCIIChar) - static_cast<int>('0');
	// Tell Controller to stop passing this event to other ModelView instances:
	return false;
}

void ModelView::initModelGeometry(vec2* vertices) // assume numVerticesInTriangle
{
	// Create the VAO and VBO names
	glGenVertexArrays(1, vao);
	glGenBuffers(1, vbo);

	// Initialize them
	glBindVertexArray(vao[0]);
	glBindBuffer(GL_ARRAY_BUFFER, vbo[0]);

	// Allocate space for AND send data to GPU
	int numBytesInBuffer = numVerticesInTriangle * sizeof(vec2);
	glBufferData(GL_ARRAY_BUFFER, numBytesInBuffer, vertices, GL_STATIC_DRAW);
	glVertexAttribPointer(shaderIF->pvaLoc("mcPosition"), 2, GL_FLOAT, GL_FALSE, 0, 0);
	glEnableVertexAttribArray(shaderIF->pvaLoc("mcPosition"));

	// Determine and remember the min/max coordinates
	xmin = xmax = vertices[0][0];
	ymin = ymax = vertices[0][1];
	for (int i=1 ; i<3 ; i++)
	{
		if (vertices[i][0] < xmin)
			xmin = vertices[i][0];
		else if (vertices[i][0] > xmax)
			xmax = vertices[i][0];
		if (vertices[i][1] < ymin)
			ymin = vertices[i][1];
		else if (vertices[i][1] > ymax)
			ymax = vertices[i][1];
	}
}

// linearMap determines the scale and translate parameters needed in
// order to map a value, f (fromMin <= f <= fromMax) to its corresponding
// value, t (toMin <= t <= toMax). Specifically: t = scale*f + trans.
void ModelView::linearMap(double fromMin, double fromMax, double toMin, double toMax,
					  double& scale, double& trans) // CLASS METHOD
{
	scale = (toMax - toMin) / (fromMax - fromMin);
	trans = toMin - scale*fromMin;
}

void ModelView::render() const
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

	// establish the current color mode and draw
	glUniform1i(shaderIF->ppuLoc("colorMode"), colorMode);

	// bind the current triangle VAO
	glBindVertexArray(vao[0]);

	// Draw the triangle.
	glDrawArrays(GL_TRIANGLES, 0, numVerticesInTriangle);

	// restore the previous program
	glUseProgram(pgm);
}

void ModelView::setMCRegionOfInterest(double xyz[6])
{
	for (int i=0 ; i<6 ; i++)
		mcRegionOfInterest[i] = xyz[i];
}
