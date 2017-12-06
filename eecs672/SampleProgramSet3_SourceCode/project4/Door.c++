// Door.c++

#include "Door.h"

PhongMaterial doorPhong(1, 1, 1, 0.5, 0.5, 0.5, 10, 1);

Door::Door(ShaderIF* sIF, cryph::AffPoint corner, double radius) :
	SceneElement(sIF, doorPhong)
{
	cryph::AffVector u(0.0, 1.0, 0.0);
	cryph::AffVector uu(u[0], u[1], 0.0), ww(0, 1, 0); uu.normalize();
	cryph::AffVector vv = ww.cross(uu);
	setTextureImage("images/door.png");

	int offset = radius;
	radius = (radius * 0.29);

	cryph::AffPoint bottom = cryph::AffPoint(corner.x, corner.y, corner.z + (offset * 0.85));
	cryph::AffPoint top = bottom + (radius * ww * 1.5);
	door = BasicShape::makeBoundedCylinder(bottom, top, radius, 20, 2, BasicShape::CAP_AT_BOTH);

	xyz[0] = 1.0; xyz[1] = 0.0;

	doorR = new BasicShapeRenderer(sIF, door);
	door->getMCBoundingBox(xyz);
}

Door::~Door() {
	if (door != nullptr)
		delete door;
	if (doorR != nullptr)
		delete doorR;
}

// xyzLimits: {mcXmin, mcXmax, mcYmin, mcYmax, mcZmin, mcZmax}
void Door::getMCBoundingBox(double* xyzLimits) const {
	for (int i = 0 ; i < 6; i++)
		xyzLimits[i] = xyz[i];
}

bool Door::handleCommand(unsigned char anASCIIChar, double ldsX, double ldsY) {
	// Building does not look for events; just hand off to inherited handleCommand.
	return this->SceneElement::handleCommand(anASCIIChar, ldsX, ldsY);
}

void Door::render() {
	// 1. Save current and establish new current shader program
	GLint pgm;
	glGetIntegerv(GL_CURRENT_PROGRAM, &pgm);
	glUseProgram(shaderIF->getShaderPgmID());

	// 2. Establish the SceneElement
	establishTexture();
	establishLightingEnvironment();
	establishView();
	establishMaterial();

	doorR->drawShape();

	glUniform1i(shaderIF->ppuLoc("textureFlag"), 0);

	// 5. Reestablish previous shader program
	glUseProgram(pgm);
}
