// TreeTop.c++

#include "TreeTop.h"

TreeTop::TreeTop(ShaderIF* sIF, PhongMaterial& matl, cryph::AffPoint point, double radius) :
	SceneElement(sIF, matl)
{
	cryph::AffVector u(0.0, 1.0, 0.0);
	cryph::AffVector uu(u[0], u[1], 0.0), ww(0, 1, 0); uu.normalize();
	cryph::AffVector vv = ww.cross(uu);

	treeTop[0] = BasicShape::makeSphere(point, radius, 20, 20);
	treeTop[1] = BasicShape::makeSphere(cryph::AffPoint(point.x + 0.5, (point.y - 0.5), point.z),
																		  (radius * 0.8), 20, 20);
	treeTop[2] = BasicShape::makeSphere(cryph::AffPoint(point.x - 0.5, (point.y - 0.5), point.z),
																			(radius * 0.8), 20, 20);

	xyz[0] = 1.0; xyz[1] = 0.0;

	for (int i = 0 ; i < 3; i++) {
		if (treeTop[i] == nullptr) {
			treeTopR[i] = nullptr;
		} else {
			treeTopR[i] = new BasicShapeRenderer(sIF, treeTop[i]);
			if (xyz[0] > xyz[1]) { // not yet initialized
				treeTop[i]->getMCBoundingBox(xyz);
			} else {
				double thisxyz[6];
				treeTop[i]->getMCBoundingBox(thisxyz);
				for (int j = 0; j < 3; j++) {
					if (thisxyz[2 * j] < xyz[2 * j])
						xyz[2 * j] = thisxyz[2 * j];
					if (thisxyz[2 * j + 1] > xyz[2 * j + 1])
						xyz[2 * j + 1] = thisxyz[2 * j + 1];
				}
			}
		}
	}
}

TreeTop::~TreeTop()
{
	for (int i = 0 ; i < 3; i++) {
		if (treeTop[i] != nullptr)
			delete treeTop[i];
		if (treeTopR[i] != nullptr)
			delete treeTopR[i];
	}
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
