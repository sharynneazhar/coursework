// LightBulb.c++

#include "LightBulb.h"

PhongMaterial white(1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0);

LightBulb::LightBulb(ShaderIF* sIF, cryph::AffPoint point) : SceneElement(sIF, white)
{
	defineBulb(point);
	lightBulbR = new BasicShapeRenderer(sIF, lightBulb);
	lightBulb->getMCBoundingBox(xyz);
}

LightBulb::~LightBulb() {
	delete lightBulb;
	delete lightBulbR;
}

void LightBulb::defineBulb(cryph::AffPoint point) {
	cryph::AffVector u(0.0, 1.0, 0.0);
	cryph::AffVector uu(u[0], u[1], 0.0), ww(0, 1, 0); uu.normalize();
	cryph::AffVector vv = ww.cross(uu);
	lightBulb = BasicShape::makeSphere(point, 0.25, 20, 20, 0, 0, 0, 0);
}

bool LightBulb::handleCommand(unsigned char anASCIIChar, double ldsX, double ldsY) {
	// Ground does not look for events; just hand off to inherited handleCommand.
	return this->SceneElement::handleCommand(anASCIIChar, ldsX, ldsY);
}

// xyzLimits: {mcXmin, mcXmax, mcYmin, mcYmax, mcZmin, mcZmax}
void LightBulb::getMCBoundingBox(double* xyzLimits) const
{
	for (int i = 0 ; i < 6; i++)
		xyzLimits[i] = xyz[i];
}

void LightBulb::render() {
	// 1. Save current and establish new current shader program
	GLint pgm;
	glGetIntegerv(GL_CURRENT_PROGRAM, &pgm);
	glUseProgram(shaderIF->getShaderPgmID());

	// 2. Establish the SceneElement
	establishTexture();
	establishLightingEnvironment();
	establishView();
	establishMaterial();

	lightBulbR->drawShape();

	// 5. Reestablish previous shader program
	glUseProgram(pgm);
}
