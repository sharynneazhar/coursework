// Tree.h

#ifndef TREE_H
#define TREE_H

#include "SceneElement.h"
#include "ShaderIF.h"
#include "Block.h"
#include "Cylinder.h"

typedef float vec3[3];

class Tree : public SceneElement
{
public:
	// As before: you will likely want to add parameters to the constructor
	Tree(ShaderIF* sIF, PhongMaterial& matl, float xMin, float yMin, float zMin);
	virtual ~Tree();

	// xyzLimits: {mcXmin, mcXmax, mcYmin, mcYmax, mcZmin, mcZmax}
	void getMCBoundingBox(double* xyzLimitsF) const;
	void render();

private:
	Cylinder* trunk;
	Block* treeTop;
	float xmin, xmax, ymin, ymax, zmin, zmax;
};

#endif
