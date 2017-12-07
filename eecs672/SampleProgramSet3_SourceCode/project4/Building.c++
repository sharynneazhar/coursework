// Building.c++

#include "Building.h"

PhongMaterial buildingPhong(0.76, 0.7, 0.5, 0.24725, 0.34615, 0.097357, 17, 1);

Building::Building(ShaderIF* sIF, cryph::AffPoint corner) :SceneElement(sIF, buildingPhong)
{
	cryph::AffVector u(0.0, 1.0, 0.0);
	cryph::AffVector uu(u[0], u[1], 0.0), ww(0, 1, 0); uu.normalize();
	cryph::AffVector vv = ww.cross(uu);

	cryph::AffPoint bottom = corner;
	cryph::AffPoint top = bottom + (2.5 * ww * 1.5);
	building[0] = BasicShape::makeBoundedCylinder(bottom, top, 4.5, 20, 2, BasicShape::CAP_AT_BOTH, 0, 20, 0, 2);
	buildingR[0] = new BasicShapeRenderer(sIF, building[0]);
	building[0]->getMCBoundingBox(xyz);
	setTextureImage("images/wall.jpg");

	door = new Door(sIF, bottom, 4.5);

	bottom = cryph::AffPoint(bottom.x, bottom.y + (top.y * 0.75), bottom.z);
	top = bottom + (4.5 * ww * 0.85);
	building[1] = BasicShape::makeBoundedCylinder(bottom, top, 5.5, 20, 2, BasicShape::CAP_AT_BOTH, 0, 1, 0, 20);
	buildingR[1] = new BasicShapeRenderer(sIF, building[1]);
	building[1]->getMCBoundingBox(xyz);

	xyz[0] = 1.0; xyz[1] = 0.0;
}

Building::~Building()
{
	delete building[0];
	delete building[1];
	delete buildingR[1];
	delete buildingR[1];
}

// xyzLimits: {mcXmin, mcXmax, mcYmin, mcYmax, mcZmin, mcZmax}
void Building::getMCBoundingBox(double* xyzLimits) const {
	for (int i = 0 ; i < 6; i++)
		xyzLimits[i] = xyz[i];
}

bool Building::handleCommand(unsigned char anASCIIChar, double ldsX, double ldsY) {
	// Building does not look for events; just hand off to inherited handleCommand.
	return this->SceneElement::handleCommand(anASCIIChar, ldsX, ldsY);
}

void Building::render() {
	// 1. Save current and establish new current shader program
	GLint pgm;
	glGetIntegerv(GL_CURRENT_PROGRAM, &pgm);
	glUseProgram(shaderIF->getShaderPgmID());

	// 2. Establish the SceneElement
	establishTexture();
	establishLightingEnvironment();
	establishView();
	establishMaterial();

	buildingR[0]->drawShape();

	glUniform1i(shaderIF->ppuLoc("sceneHasTextures"), 0);
	buildingR[1]->drawShape();

	door->render();

	// 5. Reestablish previous shader program
	glUseProgram(pgm);
}
