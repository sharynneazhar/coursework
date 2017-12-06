// Tree.h

#ifndef TREE_H
#define TREE_H

#include "SceneElement.h"
#include "AffPoint.h"
#include "AffVector.h"
#include "BasicShapeRenderer.h"
#include "TreeTop.h"

class Tree : public SceneElement
{
public:
	Tree(ShaderIF* sIF, cryph::AffPoint point);
	virtual ~Tree();

	// xyzLimits: {mcXmin, mcXmax, mcYmin, mcYmax, mcZmin, mcZmax}
	void getMCBoundingBox(double* xyzLimitsF) const;
	bool handleCommand(unsigned char anASCIIChar, double ldsX, double ldsY);
	void render();

private:
	BasicShape* trunk;
	BasicShapeRenderer* trunkR;
	TreeTop* treeTop;

	double xyz[6];

	void defineTrunk(cryph::AffPoint point, float radius);
};

#endif
