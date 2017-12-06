// TreeTop.h

#ifndef TREETOP_H
#define TREETOP_H

#include "SceneElement.h"
#include "AffPoint.h"
#include "AffVector.h"
#include "BasicShapeRenderer.h"

class TreeTop : public SceneElement
{
public:
	TreeTop(ShaderIF* sIF, cryph::AffPoint point, double radius);
	virtual ~TreeTop();

	// xyzLimits: {mcXmin, mcXmax, mcYmin, mcYmax, mcZmin, mcZmax}
	void getMCBoundingBox(double* xyzLimitsF) const;
	bool handleCommand(unsigned char anASCIIChar, double ldsX, double ldsY);
	void render();

private:
	BasicShape* treeTop[3];
	BasicShapeRenderer* treeTopR[3];

	double xyz[6];

	void defineTop(cryph::AffPoint point, float radius);
};

#endif
