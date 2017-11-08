// Crate.h

#ifndef CRATE_H
#define CRATE_H

#include "SceneElement.h"
#include "AffPoint.h"
#include "AffVector.h"
#include "BasicShapeRenderer.h"
#include "CrateTop.h"
#include "Parachute.h"

class Crate : public SceneElement
{
public:
	Crate(ShaderIF* sIF, cryph::AffPoint corner, cryph::AffVector u,
				double length, bool inAir);
	virtual ~Crate();

	// xyzLimits: {mcXmin, mcXmax, mcYmin, mcYmax, mcZmin, mcZmax}
	void getMCBoundingBox(double* xyzLimitsF) const;
	void render();

private:
	CrateTop* crateTop;
	Parachute* parachute;
	BasicShape* crate;
	BasicShapeRenderer* crateR;
	bool inAir;

	double xyz[6];
	void drawCrate();

};

#endif
