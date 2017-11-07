// Crate.c++

#include "Crate.h"

Crate::Crate(ShaderIF* sIF, PhongMaterial& matl, float xMin, float yMin, float zMin,
	float lenX, float lenY, float lenZ, bool inAirIn) : SceneElement(sIF, matl), inAir(inAirIn)
{
	PhongMaterial crateBasePhong(1, 0, 0, 1, 0, 0, 1, 0, 0, 15, 1);
	PhongMaterial crateTopPhong(0, 0, 1, 0, 0, 1, 0, 0, 1, 15, 1);

	if (inAir) {
		cryph::AffPoint parachuteBottom = cryph::AffPoint(xMin, yMin + 2.5 * lenY, zMin);
		cryph::AffPoint parachuteObj(parachuteBottom.x, parachuteBottom.y, parachuteBottom.z);

		// TODO: figure out why values less than 1 do not work
		parachute = new Parachute(sIF, parachuteObj, 1, 1);
	}

	crateBase = new Block(sIF, crateBasePhong,
												xMin, yMin, zMin,
											  lenX, lenY, lenZ);

	double xPosOffset = xMin - 0.01;
	double yPosOffset = yMin + lenY - (lenY / 3) + 0.02;
	double xLenOffset = lenX + 0.02;
	double yLenOffset = lenY / 3;
	double zLenOffset = lenZ + 0.05;

	crateTop = new Block(sIF, crateTopPhong,
											 xPosOffset, yPosOffset, zMin,
											 xLenOffset, yLenOffset, zLenOffset);

	xmin = xMin;
	xmax = xMin + lenX;
	ymin = yMin;
	ymax = yMin + lenY;
	zmin = zMin;
	zmax = zMin + lenZ;
}

Crate::~Crate()
{
	delete crateTop;
	delete crateBase;
	delete parachute;
}

// xyzLimits: {mcXmin, mcXmax, mcYmin, mcYmax, mcZmin, mcZmax}
void Crate::getMCBoundingBox(double* xyzLimits) const
{
	xyzLimits[0] = xmin;
	xyzLimits[1] = xmax;
	xyzLimits[2] = ymin;
	xyzLimits[3] = ymax;
	xyzLimits[4] = zmin;
	xyzLimits[5] = zmax;
}

void Crate::render()
{
	// 1. Save current and establish new current shader program
	GLint pgm;
	glGetIntegerv(GL_CURRENT_PROGRAM, &pgm);
	glUseProgram(shaderIF->getShaderPgmID());

	// 2. Establish "mc_ec" and "ec_lds" matrices
	SceneElement::establishView();

	// 3. Set GLSL's "kd" variable using this object's "kd" instance variable
	//    complete the implementation of SceneElement::establishMaterial and then
	//    call it from here.
	SceneElement::establishMaterial();

	// 4. Establish any other attributes and make one or more calls to
	//    glDrawArrays and/or glDrawElements
	//    If all or part of this model involves texture mapping, complete the
	//    implementation of SceneElement::establishTexture and call it from
	//    here as needed immediately before any glDrawArrays and/or glDrawElements
	//    calls to which texture is to be applied.
	crateBase->Block::render();
	crateTop->Block::render();
	if (inAir) parachute->Parachute::render();

	// 5. Reestablish previous shader program
	glUseProgram(pgm);
}
