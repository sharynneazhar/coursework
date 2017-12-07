// CrateTop.c++

#include "CrateTop.h"

PhongMaterial crateTopPhong(0.0, 0.3, 0.739, 0.5, 0.5, 0.5, 1.0, 1.0);

CrateTop::CrateTop(ShaderIF* sIF, cryph::AffPoint corner, double length) : SceneElement(sIF, crateTopPhong)
{
	cryph::AffVector u(0.0, 1.0, 0.0);
	cryph::AffVector uu(u[0], u[1], 0.0), ww(0, 0, 1); uu.normalize();
	cryph::AffVector vv = ww.cross(uu);

	double height = length * 1.005;
	double width = length * 0.3;
	double depth = length * 1.1;
	double thickness = length * 1.225;

	cryph::AffPoint cornerTop(corner.x + (length * 0.05), corner.y + (length * 0.80), corner.z - 0.15);
	cornerTop = cornerTop + height * ww;
	crateTop = BasicShape::makeBlock(cornerTop,
																	 uu, width,
																	 vv, depth,
																	 ww, thickness);

	xyz[0] = 1.0; xyz[1] = 0.0;

	crateTopR = new BasicShapeRenderer(sIF, crateTop);
	crateTop->getMCBoundingBox(xyz);
}

CrateTop::~CrateTop()
{
	if (crateTop != nullptr)
		delete crateTop;
	if (crateTopR != nullptr)
		delete crateTopR;
}

// xyzLimits: {mcXmin, mcXmax, mcYmin, mcYmax, mcZmin, mcZmax}
void CrateTop::getMCBoundingBox(double* xyzLimits) const
{
	for (int i = 0 ; i < 6; i++)
		xyzLimits[i] = xyz[i];
}

void CrateTop::render()
{
	// 1. Save current and establish new current shader program
	GLint pgm;
	glGetIntegerv(GL_CURRENT_PROGRAM, &pgm);
	glUseProgram(shaderIF->getShaderPgmID());

	// 2. Establish the SceneElement
	establishLightingEnvironment();
	establishView();
	establishMaterial();

	if (crateTopR != nullptr)
		crateTopR->drawShape();

	// 5. Reestablish previous shader program
	glUseProgram(pgm);
}
