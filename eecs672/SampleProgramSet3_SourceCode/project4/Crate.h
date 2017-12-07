// Crate.h

#ifndef CRATE_H
#define CRATE_H

#include "SceneElement.h"
#include "AffPoint.h"
#include "AffVector.h"
#include "BasicShapeRenderer.h"
#include "CrateTop.h"

class Crate : public SceneElement
{
public:
	Crate(ShaderIF* sIF, cryph::AffPoint corner, double length);
	virtual ~Crate();

	// xyzLimits: {mcXmin, mcXmax, mcYmin, mcYmax, mcZmin, mcZmax}
	void getMCBoundingBox(double* xyzLimitsF) const;
	bool handleCommand(unsigned char anASCIIChar, double ldsX, double ldsY);
	void render();

private:
	BasicShape* crate;
	BasicShapeRenderer* crateR;
	CrateTop* crateTop;
	double xyz[6];
};

#endif
