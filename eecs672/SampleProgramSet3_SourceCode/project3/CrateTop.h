// CrateTop.h

#ifndef CRATETOP_H
#define CRATETOP_H

#include "SceneElement.h"
#include "AffPoint.h"
#include "AffVector.h"
#include "BasicShapeRenderer.h"

class CrateTop : public SceneElement
{
public:
	CrateTop(ShaderIF* sIF, cryph::AffPoint corner, cryph::AffVector u, double length);
	virtual ~CrateTop();

	// xyzLimits: {mcXmin, mcXmax, mcYmin, mcYmax, mcZmin, mcZmax}
	void getMCBoundingBox(double* xyzLimitsF) const;
	void render();

private:
	BasicShape* crateTop;
	BasicShapeRenderer* crateTopR;

	double xyz[6];
	void drawCrateTop();

};

#endif
