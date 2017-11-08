// Building.c++

#include "Building.h"

PhongMaterial buildingPhong(0.91, 0.76, 0.65);

Building::Building(ShaderIF* sIF, cryph::AffPoint corner, cryph::AffVector u) :
	SceneElement(sIF, buildingPhong)
{
	cryph::AffVector uu(u[0], u[1], 0.0), ww(0, 1, 0); uu.normalize();
	cryph::AffVector vv = ww.cross(uu);

	int nPointsAroundSide = 200;
	int nPointsAlongAxis = 20;
	int radius = 4.5;

	cryph::AffPoint bottom = corner;
	cryph::AffPoint top = bottom + (radius * ww * 0.85);
	building[0] = BasicShape::makeBoundedCone(bottom, top, radius, radius,
									                          nPointsAroundSide, nPointsAlongAxis,
																						BasicShape::CAP_AT_BOTH);

	bottom = cryph::AffPoint(bottom.x, bottom.y + (top.y * 0.75), bottom.z);
	top = bottom + (radius * ww * 0.85);
	building[1] = BasicShape::makeBoundedCone(bottom, top, (radius * 1.25), (radius * 1.25),
									                          nPointsAroundSide, nPointsAlongAxis,
																						BasicShape::CAP_AT_BOTH);

	xyz[0] = 1.0; xyz[1] = 0.0;

	for (int i = 0; i < 2; i++) {
		if (building[i] == nullptr) {
			buildingR[i] = nullptr;
		} else {
			buildingR[i] = new BasicShapeRenderer(sIF, building[i]);
			if (xyz[0] > xyz[1]) {
				building[i]->getMCBoundingBox(xyz);
			} else {
				double thisxyz[6];
				building[i]->getMCBoundingBox(thisxyz);
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

Building::~Building()
{
	for (int i = 0; i < 2; i++) {
		if (building != nullptr)
			delete building[i];
		if (buildingR != nullptr)
			delete buildingR[i];
	}
}

// xyzLimits: {mcXmin, mcXmax, mcYmin, mcYmax, mcZmin, mcZmax}
void Building::getMCBoundingBox(double* xyzLimits) const
{
	for (int i = 0 ; i < 6; i++)
		xyzLimits[i] = xyz[i];
}

void Building::render()
{
	// 1. Save current and establish new current shader program
	GLint pgm;
	glGetIntegerv(GL_CURRENT_PROGRAM, &pgm);
	glUseProgram(shaderIF->getShaderPgmID());

	// 2. Establish the SceneElement
	establishLightingEnvironment();
	establishView();
	establishMaterial();

	for (int i = 0; i < 2; i++) {
		if (buildingR != nullptr)
			buildingR[i]->drawShape();
	}


	// 5. Reestablish previous shader program
	glUseProgram(pgm);
}
