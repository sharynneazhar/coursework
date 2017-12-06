// Tree.c++

#include "Tree.h"

PhongMaterial trunkPhong(0.627, 0.322, 0.176, 0.627, 0.322, 0.176, 0.5, 0.5);

Tree::Tree(ShaderIF* sIF, cryph::AffPoint point) : SceneElement(sIF, trunkPhong)
{
	float radius = 0.35;

	defineTrunk(point, radius);

	xyz[0] = 1.0; xyz[1] = 0.0;

	trunkR = new BasicShapeRenderer(shaderIF, trunk);
	trunk->getMCBoundingBox(xyz);
	// setTextureImage("images/tree-bark.png");
}

Tree::~Tree()
{
	if (trunk != nullptr)
		delete trunk;
	if (trunkR != nullptr)
		delete trunkR;
	if (treeTop != nullptr)
		delete treeTop;
}

void Tree::defineTrunk(cryph::AffPoint point, float radius) {
	cryph::AffVector u(0.0, 1.0, 0.0);
	cryph::AffVector uu(u[0], u[1], 0.0), ww(0, 1, 0); uu.normalize();
	cryph::AffVector vv = ww.cross(uu);

	cryph::AffPoint bottom = point;
	cryph::AffPoint top = bottom + (2.5 * ww);

	trunk = BasicShape::makeBoundedCylinder(bottom, top, radius,
																					10, 2, BasicShape::CAP_AT_BOTH,
																					0, 1, 0, 7);

	treeTop = new TreeTop(shaderIF, top, 0.9);
}

bool Tree::handleCommand(unsigned char anASCIIChar, double ldsX, double ldsY)
{
	// Ground does not look for events; just hand off to inherited handleCommand.
	return this->SceneElement::handleCommand(anASCIIChar, ldsX, ldsY);
}

// xyzLimits: {mcXmin, mcXmax, mcYmin, mcYmax, mcZmin, mcZmax}
void Tree::getMCBoundingBox(double* xyzLimits) const
{
	for (int i = 0 ; i < 6; i++)
		xyzLimits[i] = xyz[i];
}

void Tree::render()
{
	// 1. Save current and establish new current shader program
	GLint pgm;
	glGetIntegerv(GL_CURRENT_PROGRAM, &pgm);
	glUseProgram(shaderIF->getShaderPgmID());

	// 2. Establish the SceneElement
	establishTexture();
	establishLightingEnvironment();
	establishView();
	establishMaterial();

	trunkR->drawShape();
	treeTop->render();

	// 5. Reestablish previous shader program
	glUseProgram(pgm);
}
