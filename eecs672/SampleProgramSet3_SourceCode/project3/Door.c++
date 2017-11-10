// Door.c++

#include "Door.h"

PhongMaterial doorPhong(0.8, 0.8, 0.8, 0.5, 0.5, 0.5, 1.0, 1.0);

Door::Door(ShaderIF* sIF, cryph::AffPoint corner, cryph::AffVector u, double radius) :
	SceneElement(sIF, doorPhong)
{
	cryph::AffVector uu(u[0], u[1], 0.0), ww(0, 1, 0); uu.normalize();
	cryph::AffVector vv = ww.cross(uu);

	int nPointsAroundSide = 200;
	int nPointsAlongAxis = 20;
	int offset = radius;

	radius = (radius * 0.25);
	cryph::AffPoint bottom = cryph::AffPoint(corner.x, corner.y, corner.z + (offset * 0.85));
	cryph::AffPoint top = bottom + (radius * ww * 1.5);
	door = BasicShape::makeBoundedCylinder(bottom, top, radius,
									                   		 nPointsAroundSide, nPointsAlongAxis,
																		 	 	 BasicShape::CAP_AT_BOTH);

	xyz[0] = 1.0; xyz[1] = 0.0;
	if (door == nullptr) {
		doorR = nullptr;
	} else {
		doorR = new BasicShapeRenderer(sIF, door);
		if (xyz[0] > xyz[1]) { // not yet initialized
			door->getMCBoundingBox(xyz);
		} else {
			double thisxyz[6];
			door->getMCBoundingBox(thisxyz);
			for (int j = 0; j < 3; j++) {
				if (thisxyz[2 * j] < xyz[2 * j])
					xyz[2 * j] = thisxyz[2 * j];
				if (thisxyz[2 * j + 1] > xyz[2 * j + 1])
					xyz[2 * j + 1] = thisxyz[2 * j + 1];
			}
		}
	}

}

Door::~Door()
{
	if (door != nullptr)
		delete door;
	if (doorR != nullptr)
		delete doorR;
}

// xyzLimits: {mcXmin, mcXmax, mcYmin, mcYmax, mcZmin, mcZmax}
void Door::getMCBoundingBox(double* xyzLimits) const
{
	for (int i = 0 ; i < 6; i++)
		xyzLimits[i] = xyz[i];
}

void Door::render()
{
	// 1. Save current and establish new current shader program
	GLint pgm;
	glGetIntegerv(GL_CURRENT_PROGRAM, &pgm);
	glUseProgram(shaderIF->getShaderPgmID());

	// 2. Establish the SceneElement
	establishLightingEnvironment();
	establishView();
	establishMaterial();

	if (doorR != nullptr)
		doorR->drawShape();

	// 5. Reestablish previous shader program
	glUseProgram(pgm);
}
