// Parachute.c++

#include "Parachute.h"

PhongMaterial parachutePhong(0.19225, 0.19225, 0.19225,
	                           0.50754, 0.50754, 0.50754,
														 0.508273, 0.508273, 0.508273,
														 51.2, 1.0);

Parachute::Parachute(ShaderIF* sIF, cryph::AffPoint corner, cryph::AffVector u, double radius) :
	SceneElement(sIF, parachutePhong)
{
	cryph::AffVector uu(u[0], u[1], 0.0), ww(0, 0, 1); uu.normalize();
	cryph::AffVector vv = ww.cross(uu);

	int nPointsAroundSide = 200;
	int nPointsAlongAxis = 20;
	cryph::AffPoint cornerParachute(corner.x + (radius * 1.25), corner.y, corner.z);
	cryph::AffPoint bottom = cornerParachute + (2.0 * radius * (uu + vv));
	cryph::AffPoint top = bottom + (radius * ww * 1.15);

	parachute = BasicShape::makeBoundedCone(
										bottom, top, 1, radius,
										nPointsAroundSide, nPointsAlongAxis,
										BasicShape::CAP_AT_BOTH);

	xyz[0] = 1.0; xyz[1] = 0.0;
	if (parachute == nullptr) {
		parachuteR = nullptr;
	} else {
		parachuteR = new BasicShapeRenderer(sIF, parachute);
		if (xyz[0] > xyz[1]) {
			parachute->getMCBoundingBox(xyz);
		} else {
			double thisxyz[6];
			parachute->getMCBoundingBox(thisxyz);
			for (int j = 0; j < 3; j++) {
				if (thisxyz[2 * j] < xyz[2 * j])
					xyz[2 * j] = thisxyz[2 * j];
				if (thisxyz[2 * j + 1] > xyz[2 * j + 1])
					xyz[2 * j + 1] = thisxyz[2 * j + 1];
			}
		}
	}

}

Parachute::~Parachute()
{
	if (parachute != nullptr)
		delete parachute;
	if (parachuteR != nullptr)
		delete parachuteR;
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

	if (parachuteR != nullptr)
		parachuteR->drawShape();

	// 5. Reestablish previous shader program
	glUseProgram(pgm);
}
