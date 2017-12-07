// Puddle.c++

#include "Puddle.h"

PhongMaterial puddlePhong(0.25, 0.5, 1, 0.1, 0.396, 0.097254, 2, 0.25);

Puddle::Puddle(ShaderIF* sIF, cryph::AffPoint corner, double radius) :SceneElement(sIF, puddlePhong)
{
	cryph::AffVector u(0.0, 1.0, 0.0);
	cryph::AffVector uu(u[0], u[1], 0.0), ww(0, 1, 0); uu.normalize();
	cryph::AffVector vv = ww.cross(uu);

	puddle = BasicShape::makeBoundedCone(corner, corner + (0.35 * ww), radius, 0.01,
																			 10, 2, BasicShape::CAP_AT_BOTTOM,
																			 0, 3, 0, 1);

	puddleR = new BasicShapeRenderer(sIF, puddle);
	puddle->getMCBoundingBox(xyz);
	setTextureImage("textures/water.jpg");
}

Puddle::~Puddle()
{
	delete puddle;
	delete puddleR;
}

// xyzLimits: {mcXmin, mcXmax, mcYmin, mcYmax, mcZmin, mcZmax}
void Puddle::getMCBoundingBox(double* xyzLimits) const {
	for (int i = 0 ; i < 6; i++)
		xyzLimits[i] = xyz[i];
}

bool Puddle::handleCommand(unsigned char anASCIIChar, double ldsX, double ldsY) {
	// Puddle does not look for events; just hand off to inherited handleCommand.
	return this->SceneElement::handleCommand(anASCIIChar, ldsX, ldsY);
}

void Puddle::render() {
	ProjController* c = dynamic_cast<ProjController*>(Controller::getCurrentController());
	if (c->drawingOpaque()) {
		return;
	}

	// 1. Save current and establish new current shader program
	GLint pgm;
	glGetIntegerv(GL_CURRENT_PROGRAM, &pgm);
	glUseProgram(shaderIF->getShaderPgmID());


	// 2. Establish the SceneElement
	establishTexture();
	establishLightingEnvironment();
	establishView();
	establishMaterial();

	glUniform1f(shaderIF->ppuLoc("alpha"), 0.65);
	puddleR->drawShape();

	// 5. Reestablish previous shader program
	glUseProgram(pgm);
}
