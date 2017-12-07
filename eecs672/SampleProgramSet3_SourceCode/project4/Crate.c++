// Crate.c++

#include "Crate.h"

PhongMaterial crateBasePhong(1.0, 0.0, 0.0, 0.5, 0.5, 0.5, 0.5, 0.5);

Crate::Crate(ShaderIF* sIF, cryph::AffPoint corner, double length) :
	SceneElement(sIF, crateBasePhong)
{
	cryph::AffVector u(0.0, 1.0, 0.0);
	cryph::AffVector uu(u[0], u[1], 0.0), ww(0, 0, 1); uu.normalize();
	cryph::AffVector vv = ww.cross(uu);

	crate = BasicShape::makeBlock(corner + length * ww,
																uu, length,
																vv, length,
																ww, length);

	crateTop = new CrateTop(sIF, corner, length);

	xyz[0] = 1.0; xyz[1] = 0.0;

	crateR = new BasicShapeRenderer(sIF, crate);
	crate->getMCBoundingBox(xyz);
}

Crate::~Crate() {
	if (crate != nullptr)
		delete crate;
	if (crateR != nullptr)
		delete crateR;
}

// xyzLimits: {mcXmin, mcXmax, mcYmin, mcYmax, mcZmin, mcZmax}
void Crate::getMCBoundingBox(double* xyzLimits) const {
	for (int i = 0 ; i < 6; i++)
		xyzLimits[i] = xyz[i];
}

bool Crate::handleCommand(unsigned char anASCIIChar, double ldsX, double ldsY) {
	// Ground does not look for events; just hand off to inherited handleCommand.
	return this->SceneElement::handleCommand(anASCIIChar, ldsX, ldsY);
}

void Crate::render() {
	// 1. Save current and establish new current shader program
	GLint pgm;
	glGetIntegerv(GL_CURRENT_PROGRAM, &pgm);
	glUseProgram(shaderIF->getShaderPgmID());

	// 2. Establish the SceneElement
	glUniform1i(shaderIF->ppuLoc("sceneHasTextures"), 0);
	establishLightingEnvironment();
	establishView();
	establishMaterial();

	if (crateR != nullptr) {
		crateR->drawShape();
		crateTop->render();
	}

	// 5. Reestablish previous shader program
	glUseProgram(pgm);
}
