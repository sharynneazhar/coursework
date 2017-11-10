// Parachute.c++

#include "Parachute.h"

PhongMaterial parachutePhong(0.105882, 0.058824, 0.113725,
														 0.427451, 0.470588, 0.541176,
														 0.333333, 0.333333, 0.521569,
														 9.84615, 1.0);

Parachute::Parachute(ShaderIF* sIF, cryph::AffPoint corner, cryph::AffVector u, double length) :
	SceneElement(sIF, parachutePhong)
{
	cryph::AffVector uu(u[0], u[1], 0.0), ww(0, 1, 0); uu.normalize();
	cryph::AffVector vv = ww.cross(uu);

	int nPointsAroundSide = 200;
	int nPointsAlongAxis = 2;
	int radius = length * 1.75;

	cryph::AffPoint cornerParachute(corner.x - (radius * 0.5), corner.y + (radius * 0.25), corner.z);
	cryph::AffPoint bottom = cornerParachute + (2.0 * length * (uu + vv));
	cryph::AffPoint top = bottom + (radius * ww * 0.35);
	parachute[0] = BasicShape::makeBoundedCone(
										bottom, top, radius, 0.001,
										nPointsAroundSide, nPointsAlongAxis,
										BasicShape::CAP_AT_TOP);

	cryph::AffPoint stringBottom1(corner.x - (length * 0.5),
																corner.y + (length * 0.25),
																corner.z + (length * 1.8));
	cryph::AffPoint stringTop1(bottom.x - (radius * 0.9), bottom.y, bottom.z);
	parachute[1] = BasicShape::makeBoundedCylinder(
										stringBottom1, stringTop1, 0.05,
										nPointsAroundSide, nPointsAlongAxis,
										BasicShape::CAP_AT_BOTH);

	cryph::AffPoint stringBottom2(corner.x + (length * 0.5),
																corner.y + (length * 0.25),
																corner.z + (length * 1.8));
	cryph::AffPoint stringTop2(bottom.x + (radius * 0.9), bottom.y, bottom.z);
	parachute[2] = BasicShape::makeBoundedCylinder(
										stringBottom1, stringTop2, 0.05,
										nPointsAroundSide, nPointsAlongAxis,
										BasicShape::CAP_AT_BOTH);

	xyz[0] = 1.0; xyz[1] = 0.0;
	for (int i = 0; i < 3; i++) {
		if (parachute[i] == nullptr) {
			parachuteR[i] = nullptr;
		} else {
			parachuteR[i] = new BasicShapeRenderer(sIF, parachute[i]);
			if (xyz[0] > xyz[1]) {
				parachute[i]->getMCBoundingBox(xyz);
			} else {
				double thisxyz[6];
				parachute[i]->getMCBoundingBox(thisxyz);
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

Parachute::~Parachute()
{
	for (int i = 0; i < 3; i++) {
		if (parachute[i] != nullptr)
			delete parachute[i];
		if (parachuteR[i] != nullptr)
			delete parachuteR[i];
	}
}

// xyzLimits: {mcXmin, mcXmax, mcYmin, mcYmax, mcZmin, mcZmax}
void Parachute::getMCBoundingBox(double* xyzLimits) const
{
	for (int i = 0 ; i < 6; i++)
		xyzLimits[i] = xyz[i];
}

void Parachute::render()
{
	// 1. Save current and establish new current shader program
	GLint pgm;
	glGetIntegerv(GL_CURRENT_PROGRAM, &pgm);
	glUseProgram(shaderIF->getShaderPgmID());

	// 2. Establish the SceneElement
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
