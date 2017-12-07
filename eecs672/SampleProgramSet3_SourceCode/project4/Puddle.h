// Puddle.h

#ifndef PUDDLE_H
#define PUDDLE_H

#include "ProjController.h"
#include "SceneElement.h"
#include "AffPoint.h"
#include "AffVector.h"
#include "BasicShapeRenderer.h"

class Puddle : public SceneElement
{
public:
	Puddle(ShaderIF* sIF, cryph::AffPoint corner, double radius);
	virtual ~Puddle();

	// xyzLimits: {mcXmin, mcXmax, mcYmin, mcYmax, mcZmin, mcZmax}
	void getMCBoundingBox(double* xyzLimitsF) const;
	bool handleCommand(unsigned char anASCIIChar, double ldsX, double ldsY);
	void render();

private:
	BasicShape* puddle;
	BasicShapeRenderer* puddleR;
	double xyz[6];
};

#endif
