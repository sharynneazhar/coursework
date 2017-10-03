// Cylinder.c++

#include <iostream>
#include <math.h>

#include "Cylinder.h"

Cylinder::Cylinder(ShaderIF* sIF) : shaderIF(sIF)
{
	kd[0] = 0.25; kd[1] = 1.0; kd[2] = 0.5;
	defineCylinder();
}

Cylinder::~Cylinder()
{
	glDeleteBuffers(2, vbo);
	glDeleteVertexArrays(1, vao);
}

const int N_POINTS_AROUND_SLICE = 18; // number points around a cross-section

void Cylinder::defineCylinder()
{
	typedef float vec3[3];
	int nPoints = 2 * (N_POINTS_AROUND_SLICE + 1);
	vec3* coords = new vec3[nPoints];
	vec3* normals = new vec3[nPoints];
	double theta = 0.0;
	double dTheta = 2.0*M_PI/N_POINTS_AROUND_SLICE;
	double yb = -0.35, zb = 1.4;
	double x1 = 0.6, x2 = 1.1;
	double r = 0.05;
	// EXERCISE: Use cryph to generalize this to define a cylinder in
	//           an arbitrary position and orientation.
	for (int i=0 ; i<=N_POINTS_AROUND_SLICE ; i++)
	{
		int j = 2 * i;
		double dy = cos(theta);
		double dz = sin(theta);
		normals[j][0] = 0.0; normals[j][1] = dy; normals[j][2] = dz;
		coords[j][0] = x1; coords[j][1] = yb + r*dy; coords[j][2] = zb + r*dz;
		normals[j+1][0] = 0.0; normals[j+1][1] = dy; normals[j+1][2] = dz;
		coords[j+1][0] = x2; coords[j+1][1] = yb + r*dy; coords[j+1][2] = zb + r*dz;
		theta += dTheta;
	}

	// ************************************************************************
	// EXERCISE: Create/fill VAOs and VBOs here.
	//           Also use glVertexAttribPointer and glEnableVertexAttribArray
	// ************************************************************************
	std::cout << "Cylinder::defineCylinder: create/fill VBOs. You will see errors until you do so.\n";

	delete [] coords;
	delete [] normals;
}

// xyzLimits: {mcXmin, mcXmax, mcYmin, mcYmax, mcZmin, mcZmax}
void Cylinder::getMCBoundingBox(double* xyzLimits) const
{
	xyzLimits[0] = -1.0;
	xyzLimits[1] = 1.0;
	xyzLimits[2] = -1.0;
	xyzLimits[3] = 1.0;
	xyzLimits[4] = -1.0;
	xyzLimits[5] = 1.0;
}

bool Cylinder::handleCommand(unsigned char anASCIIChar, double ldsX, double ldsY)
{
	// Cylinder does not look for events. Just hand off back to inherited handleCommand.
	return this->ModelView::handleCommand(anASCIIChar, ldsX, ldsY);
}

void Cylinder::render()
{
	typedef float vec3[3];
	GLint pgm;
	glGetIntegerv(GL_CURRENT_PROGRAM, &pgm);
	glUseProgram(shaderIF->getShaderPgmID());

	cryph::Matrix4x4 mc_ec, ec_lds;
	getMatrices(mc_ec, ec_lds);
	float mat[16];
	glUniformMatrix4fv(shaderIF->ppuLoc("mc_ec"), 1, false, mc_ec.extractColMajor(mat));
	glUniformMatrix4fv(shaderIF->ppuLoc("ec_lds"), 1, false, ec_lds.extractColMajor(mat));

	glUniform3fv(shaderIF->ppuLoc("kd"), 1, kd);
	glBindVertexArray(vao[0]);
	glDrawArrays(GL_TRIANGLE_STRIP, 0, 2*(N_POINTS_AROUND_SLICE+1));

	glUseProgram(pgm);
}
