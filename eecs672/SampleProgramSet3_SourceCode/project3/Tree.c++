// Tree.c++

#include "Tree.h"

Tree::Tree(ShaderIF* sIF, PhongMaterial& matl, float xMin, float yMin, float zMin) : SceneElement(sIF, matl)
{
	vec3 trunkColor = {1, 1, 1};

	PhongMaterial trunkPhong(1, 1, 1, 1, 1, 1, 1, 1, 1, 15, 1);
	PhongMaterial treeTopPhong(0, 1, 0, 0, 1, 0, 0, 1, 0, 15, 1);

	trunk = new Cylinder(sIF, xMin, yMin, zMin, 0.0, 0.0, 90.0, 0.25, 2.3, 1.0, trunkColor);

	if (xMin < 0)
		treeTop = new Block(sIF, treeTopPhong, xMin - 0.18, yMin + 2.3, zMin, 1.0, 1.0, 1.0);
	else
		treeTop = new Block(sIF, treeTopPhong, xMin - 0.36, yMin + 2.3, zMin, 1.0, 1.0, 1.0);
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
	SceneElement::establishView();

	// 3. Set GLSL's "kd" variable using this object's "kd" instance variable
	//    complete the implementation of SceneElement::establishMaterial and then
	//    call it from here.
	SceneElement::establishMaterial();

	// 4. Establish any other attributes and make one or more calls to
	//    glDrawArrays and/or glDrawElements
	//    If all or part of this model involves texture mapping, complete the
	//    implementation of SceneElement::establishTexture and call it from
	//    here as needed immediately before any glDrawArrays and/or glDrawElements
	//    calls to which texture is to be applied.
	trunk->Cylinder::render();
	treeTop->Block::render();

	// 5. Reestablish previous shader program
	glUseProgram(pgm);
}
