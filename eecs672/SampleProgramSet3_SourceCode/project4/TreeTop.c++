// TreeTop.c++

#include "TreeTop.h"

PhongMaterial forestGreen(0.419608, 0.656863, 0.137255, 0.5, 0.5, 0.5, 1.0, 1.0);

TreeTop::TreeTop(ShaderIF* sIF, cryph::AffPoint point, double radius) : SceneElement(sIF, forestGreen)
{
	defineTop(point, radius);
	xyz[0] = 1.0; xyz[1] = 0.0;

	for (int i = 0 ; i < 3; i++) {
		treeTopR[i] = new BasicShapeRenderer(sIF, treeTop[i]);
		treeTop[i]->getMCBoundingBox(xyz);
		// setTextureImage("images/tree-leaves.jpg");
	}
}

TreeTop::~TreeTop() {
	for (int i = 0; i < 3; i++) {
		delete treeTop[i];
		delete treeTopR[i];
	}
}

void TreeTop::defineTop(cryph::AffPoint point, float radius) {
	cryph::AffVector u(0.0, 1.0, 0.0);
	cryph::AffVector uu(u[0], u[1], 0.0), ww(0, 1, 0); uu.normalize();
	cryph::AffVector vv = ww.cross(uu);

	cryph::AffPoint side1 = cryph::AffPoint(point.x + 0.5, (point.y - 0.35), point.z);
	cryph::AffPoint side2 = cryph::AffPoint(point.x - 0.5, (point.y - 0.35), point.z);
	treeTop[0] = BasicShape::makeSphere(point, radius, 20, 20, 0, 4.5, 0, 2.75);
	treeTop[1] = BasicShape::makeSphere(side1, (radius * 0.8), 20, 20, 0, 4.5, 0, 2.75);
	treeTop[2] = BasicShape::makeSphere(side2, (radius * 0.8), 20, 20, 0, 4.5, 0, 2.75);
}

bool TreeTop::handleCommand(unsigned char anASCIIChar, double ldsX, double ldsY) {
	// Ground does not look for events; just hand off to inherited handleCommand.
	return this->SceneElement::handleCommand(anASCIIChar, ldsX, ldsY);
}

// xyzLimits: {mcXmin, mcXmax, mcYmin, mcYmax, mcZmin, mcZmax}
void TreeTop::getMCBoundingBox(double* xyzLimits) const
{
	for (int i = 0 ; i < 6; i++)
		xyzLimits[i] = xyz[i];
}

void TreeTop::render()
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

	for (int i = 0 ; i < 3; i++) {
		if (treeTopR[i] != nullptr)
			treeTopR[i]->drawShape();
	}

	// 5. Reestablish previous shader program
	glUseProgram(pgm);
}
