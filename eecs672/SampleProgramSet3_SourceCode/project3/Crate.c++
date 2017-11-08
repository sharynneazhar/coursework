// Crate.c++

#include "Crate.h"

PhongMaterial crateBasePhong(0.739, 0.0, 0.0, 0.65, 0.65, 0.65, 1.0, 1.0);

Crate::Crate(ShaderIF* sIF, cryph::AffPoint corner, cryph::AffVector u,
	double length, bool inAirIn) : SceneElement(sIF, crateBasePhong), inAir(inAirIn)
{
	cryph::AffVector uu(u[0], u[1], 0.0), ww(0, 0, 1); uu.normalize();
	cryph::AffVector vv = ww.cross(uu);

	double height = length;
	double width = length;
	double depth = length;
	double thickness = length;

	crate = BasicShape::makeBlock(corner + height * ww,
																uu, width,
																vv, depth,
																ww, thickness);

	crateTop = new CrateTop(sIF, corner, u, length);

	if (inAir)
		parachute = new Parachute(sIF, corner, u, length);

	xyz[0] = 1.0; xyz[1] = 0.0;

	if (crate == nullptr) {
		crateR = nullptr;
	} else {
		crateR = new BasicShapeRenderer(sIF, crate);
		if (xyz[0] > xyz[1]) { // not yet initialized
			crate->getMCBoundingBox(xyz);
		} else {
			double thisxyz[6];
			crate->getMCBoundingBox(thisxyz);
			for (int j = 0; j < 3; j++) {
				if (thisxyz[2 * j] < xyz[2 * j])
					xyz[2 * j] = thisxyz[2 * j];
				if (thisxyz[2 * j + 1] > xyz[2 * j + 1])
					xyz[2 * j + 1] = thisxyz[2 * j + 1];
			}
		}
	}
}

Crate::~Crate()
{
	if (crate != nullptr)
		delete crate;
	if (crateR != nullptr)
		delete crateR;
}

// xyzLimits: {mcXmin, mcXmax, mcYmin, mcYmax, mcZmin, mcZmax}
void Crate::getMCBoundingBox(double* xyzLimits) const
{
	for (int i = 0 ; i < 6; i++)
		xyzLimits[i] = xyz[i];
}

void Crate::render()
{
	// 1. Save current and establish new current shader program
	GLint pgm;
	glGetIntegerv(GL_CURRENT_PROGRAM, &pgm);
	glUseProgram(shaderIF->getShaderPgmID());

	// 2. Establish the SceneElement
	establishLightingEnvironment();
	establishView();
	establishMaterial();

	if (crateR != nullptr) {
		crateR->drawShape();
		crateTop->render();

		// if (inAir)
		// 	parachute->render();
	}


	// 5. Reestablish previous shader program
	glUseProgram(pgm);
}
