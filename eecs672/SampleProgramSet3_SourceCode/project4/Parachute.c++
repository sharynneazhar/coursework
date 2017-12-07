// Parachute.c++

#include "Parachute.h"

PhongMaterial parachutePhong(0.0, 0.1, 0.0, 0.3, 0.3, 0.3, 12, 1);

Parachute::Parachute(ShaderIF* sIF, cryph::AffPoint corner, double length) :
	SceneElement(sIF, parachutePhong)
{
	cryph::AffVector u(0.0, 1.0, 0.0);
	cryph::AffVector uu(u[0], u[1], 0.0), ww(0, 1, 0); uu.normalize();
	cryph::AffVector vv = ww.cross(uu);

	double radius = length * 1.85;

	// Draw the parachute
	cryph::AffPoint cornerParachute(corner.x - (radius * 0.275), corner.y + (radius * 0.25), corner.z + 1.25);
	cryph::AffPoint bottom = cornerParachute + (2.0 * length * (uu + vv));
	cryph::AffPoint top = bottom + (radius * ww * 0.35);
	parachute[0] = BasicShape::makeBoundedCone(bottom, top, radius, 0.15, 20, 2, BasicShape::CAP_AT_TOP, 0, 3, 0, 1.5);

	// Draw the parachute strings
	cryph::AffPoint stringBottom1(corner.x - (length * 0.5), corner.y + (length * 0.25), corner.z + (length * 1.8));
	cryph::AffPoint stringTop1(bottom.x - (radius * 0.9), bottom.y, bottom.z);
	parachute[1] = BasicShape::makeBoundedCylinder(stringBottom1, stringTop1, 0.05, 20, 2, BasicShape::CAP_AT_BOTH);

	cryph::AffPoint stringBottom2(corner.x + (length * 0.5), corner.y + (length * 0.25), corner.z + (length * 1.8));
	cryph::AffPoint stringTop2(bottom.x + (radius * 0.9), bottom.y, bottom.z);
	parachute[2] = BasicShape::makeBoundedCylinder(stringBottom1, stringTop2, 0.05, 20, 2, BasicShape::CAP_AT_BOTH);

	xyz[0] = 1.0; xyz[1] = 0.0;
	for (int i = 0; i < 3; i++) {
		parachuteR[i] = new BasicShapeRenderer(sIF, parachute[i]);
		parachute[i]->getMCBoundingBox(xyz);
		setTextureImage("images/parachute.jpg");
	}
}

Parachute::~Parachute() {
	for (int i = 0; i < 3; i++) {
		if (parachute[i] != nullptr)
			delete parachute[i];
		if (parachuteR[i] != nullptr)
			delete parachuteR[i];
	}
}

// xyzLimits: {mcXmin, mcXmax, mcYmin, mcYmax, mcZmin, mcZmax}
void Parachute::getMCBoundingBox(double* xyzLimits) const {
	for (int i = 0 ; i < 6; i++)
		xyzLimits[i] = xyz[i];
}

bool Parachute::handleCommand(unsigned char anASCIIChar, double ldsX, double ldsY) {
	// Building does not look for events; just hand off to inherited handleCommand.
	return this->SceneElement::handleCommand(anASCIIChar, ldsX, ldsY);
}

void Parachute::render() {
	// 1. Save current and establish new current shader program
	GLint pgm;
	glGetIntegerv(GL_CURRENT_PROGRAM, &pgm);
	glUseProgram(shaderIF->getShaderPgmID());

	// 2. Establish the SceneElement
	establishTexture();
	establishLightingEnvironment();
	establishView();
	establishMaterial();

	for (int i = 0; i < 3; i++) {
		if (parachuteR[i] != nullptr)
			parachuteR[i]->drawShape();
	}

	// 5. Reestablish previous shader program
	glUseProgram(pgm);
}
