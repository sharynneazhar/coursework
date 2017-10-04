// Tree.c++ - concrete subclass of ModelView that represents a Tree

#include <iostream>
#include <cmath>

#include "Tree.h"

typedef float vec2[2];

Tree::Tree(ShaderIF* sIF,
	double xbIn, double ybIn, double rTreeTopIn, double rTrunkIn, double heightIn,
	int numTreeTopPointsIn, int numTrunkPointsIn) : shaderIF(sIF),
	xb(xbIn), yb(ybIn), rTreeTop(rTreeTopIn), rTrunk(rTrunkIn), height(heightIn),
	numTreeTopPoints(numTreeTopPointsIn), numTrunkPoints(numTrunkPointsIn)
{
	validateData();
	defineModel();
}

Tree::~Tree()
{
	glDeleteBuffers(1, vbo);
	glDeleteVertexArrays(1, vao);
}

void Tree::defineModel()
{
	vec2 points[numTrunkPoints];
	double trunkTheta = asin(height / rTrunk);

	for (int i = 0; i < numTrunkPoints; i += 2) {
		// draw the left side of the trunk based on the center (xb, yb) given
		points[i][0] = xb - rTrunk * (1.0 - cos(trunkTheta));
		points[i][1] = yb + height * (1.0 - sin(trunkTheta));
		points[i + 1][0] = 2.0 * xb - points[i][0];
		points[i + 1][1] = points[i][1];
	}

	// send the data to GPU
	glGenVertexArrays(1, vao);
	glBindVertexArray(vao[0]);

	glGenBuffers(1, vbo);
	glBindBuffer(GL_ARRAY_BUFFER, vbo[0]);
	glBufferData(GL_ARRAY_BUFFER, numTrunkPoints*sizeof(vec2), points, GL_STATIC_DRAW);
	glVertexAttribPointer(shaderIF->pvaLoc("mcPosition"), 2, GL_FLOAT, GL_FALSE, 0, 0);
	glEnableVertexAttribArray(shaderIF->pvaLoc("mcPosition"));
}

// xyzLimits: {mcXmin, mcXmax, mcYmin, mcYmax, mcZmin, mcZmax}
void Tree::getMCBoundingBox(double* xyzLimits) const
{
	double dx1 = xb - rTreeTop; // min x of tree top
	double trunkTheta = asin(height/rTrunk);
	double dx2 = xb - rTrunk*(1.0 - cos(trunkTheta)); // min x of base
	if (dx1 < dx2)
		xyzLimits[0] = dx1;
	else
		xyzLimits[0] = dx2;
	xyzLimits[1] = 2.0*xb - xyzLimits[0];
	xyzLimits[2] = yb;
	xyzLimits[3] = yb + height + rTreeTop;
	xyzLimits[4] = -1.0; xyzLimits[5] = 1.0; // (zmin, zmax) (really 0..0)
}

void Tree::render()
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
	// draw trunk first because tree top overwrites it
	glUniform1i(shaderIF->ppuLoc("treePart"), 0); // '0' means trunk
	int numTrunkTriStripPoints = 2 * numTrunkPoints;
	glDrawArrays(GL_TRIANGLE_STRIP, 0, numTrunkTriStripPoints); // offset: 0
	// then draw tree top
	glUniform1i(shaderIF->ppuLoc("treePart"), 1); // '1' means tree top
	//                            offset:                 num points:
	glDrawArrays(GL_TRIANGLE_FAN, numTrunkTriStripPoints, numTreeTopPoints);
	// restore the previous program
	glUseProgram(pgm);
}

void Tree::validateData()
{
	if (rTreeTop <= 0.0)
		rTreeTop = 1.0;
	if (height <= 0.0)
		height = 1.0;
	if (rTrunk < height)
		rTrunk = height;
	if (numTreeTopPoints < 5)
		numTreeTopPoints = 5;
	if (numTrunkPoints < 2)
		numTrunkPoints = 2;
}
