// LightBulb.h

#ifndef LIGHTBULB_H
#define LIGHTBULB_H

#include "SceneElement.h"
#include "AffPoint.h"
#include "AffVector.h"
#include "BasicShapeRenderer.h"

class LightBulb : public SceneElement
{
public:
	LightBulb(ShaderIF* sIF, cryph::AffPoint point);
	virtual ~LightBulb();

	// xyzLimits: {mcXmin, mcXmax, mcYmin, mcYmax, mcZmin, mcZmax}
	void getMCBoundingBox(double* xyzLimitsF) const;
	bool handleCommand(unsigned char anASCIIChar, double ldsX, double ldsY);
	void render();

private:
	BasicShape* lightBulb;
	BasicShapeRenderer* lightBulbR;

	double xyz[6];

	void defineBulb(cryph::AffPoint point);
};

#endif
