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
	TreeTop(ShaderIF* sIF, PhongMaterial& matl, cryph::AffPoint point, double radius);
	virtual ~TreeTop();

	// xyzLimits: {mcXmin, mcXmax, mcYmin, mcYmax, mcZmin, mcZmax}
	void getMCBoundingBox(double* xyzLimitsF) const;
	void render();

private:
	BasicShape* treeTop[3];
	BasicShapeRenderer* treeTopR[3];

	double xyz[6];
	void drawTreeTop();
};

#endif
