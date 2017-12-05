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
	Tree(ShaderIF* sIF, cryph::AffPoint point, double height);
	virtual ~Tree();

	// xyzLimits: {mcXmin, mcXmax, mcYmin, mcYmax, mcZmin, mcZmax}
	void getMCBoundingBox(double* xyzLimitsF) const;
	void render();

private:
	TreeTop* treeTop;
	BasicShape* tree;
	BasicShapeRenderer* treeR;

	double xyz[6];
	void drawTree();

};

#endif
