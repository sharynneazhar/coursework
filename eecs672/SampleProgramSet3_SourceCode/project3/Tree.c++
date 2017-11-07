// Tree.c++

#include "Tree.h"

Tree::Tree(ShaderIF* sIF, float xMin, float yMin, float zMin) : shaderIF(sIF)
{
	vec3 trunkColor = { 0.52, 0.37, 0.26 };
	vec3 treeTopColor = { 0.13, 0.37, 0.31};

	trunk = new Cylinder(sIF, xMin, yMin, zMin, 0.0, 0.0, 90.0, 0.25, 2.3, 1.0, trunkColor);

	if (xMin < 0)
		treeTop = new Block(sIF, xMin - 0.18, yMin + 2.3, zMin, 1.0, 1.0, 1.0, treeTopColor);
	else
		treeTop = new Block(sIF, xMin - 0.36, yMin + 2.3, zMin, 1.0, 1.0, 1.0, treeTopColor);
}

Tree::~Tree()
{
	delete trunk;
	delete treeTop;
}

// xyzLimits: {mcXmin, mcXmax, mcYmin, mcYmax, mcZmin, mcZmax}
void Tree::getMCBoundingBox(double* xyzLimits) const
{
	xyzLimits[0] = xmin;
	xyzLimits[1] = xmax;
	xyzLimits[2] = ymin;
	xyzLimits[3] = ymax;
	xyzLimits[4] = zmin;
	xyzLimits[5] = zmax;
}

void Tree::render()
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
	trunk->Cylinder::render();
	treeTop->Block::render();

	// 5. Reestablish previous shader program
	glUseProgram(pgm);
}
