// Ground.c++

#include "Ground.h"

Ground::Ground(ShaderIF* sIF, float xMin, float yMin, float zMin,
	float lenX, float lenY, float lenZ, vec3 color) : shaderIF(sIF)
{
	groundBlock = new Block(sIF, xMin, yMin, zMin, lenX, lenY, lenZ, color);
	xmin = xMin;
	xmax = xMin + lenX;
	ymin = yMin;
	ymax = yMin + lenY;
	zmin = zMin;
	zmax = zMin + lenZ;
}

Ground::~Ground()
{
	delete groundBlock;
}

// xyzLimits: {mcXmin, mcXmax, mcYmin, mcYmax, mcZmin, mcZmax}
void Ground::getMCBoundingBox(double* xyzLimits) const
{
	xyzLimits[0] = xmin;
	xyzLimits[1] = xmax;
	xyzLimits[2] = ymin;
	xyzLimits[3] = ymax;
	xyzLimits[4] = zmin;
	xyzLimits[5] = zmax;
}

void Ground::render()
{
	// 1. Save current and establish new current shader program
	GLint pgm;
	glGetIntegerv(GL_CURRENT_PROGRAM, &pgm);
	glUseProgram(shaderIF->getShaderPgmID());

	// 2. Establish "mc_ec" and "ec_lds" matrices
	cryph::Matrix4x4 mc_ec, ec_lds;
	getMatrices(mc_ec, ec_lds);

	float mat[16];
	glUniformMatrix4fv(shaderIF->ppuLoc("mc_ec"), 1, false, mc_ec.extractColMajor(mat));
	glUniformMatrix4fv(shaderIF->ppuLoc("ec_lds"), 1, false, ec_lds.extractColMajor(mat));

	glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);

	// 3. Set GLSL's "kd" variable using this object's "kd" instance variable
	glUniform3fv(shaderIF->ppuLoc("kd"), 1, kd);

	// 4. Establish any other attributes and make one or more calls to
	//    glDrawArrays and/or glDrawElements
	groundBlock->Block::render();

	// 5. Reestablish previous shader program
	glUseProgram(pgm);
}
