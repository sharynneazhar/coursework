// Tree.h

#ifndef TREE_H
#define TREE_H

#include "ModelView.h"
#include "ShaderIF.h"
#include "Block.h"
#include "Cylinder.h"

typedef float vec3[3];

class Tree : public ModelView
{
public:
	// As before: you will likely want to add parameters to the constructor
	Tree(ShaderIF* sIF, float xMin, float yMin, float zMin);
	virtual ~Tree();

	// xyzLimits: {mcXmin, mcXmax, mcYmin, mcYmax, mcZmin, mcZmax}
	void getMCBoundingBox(double* xyzLimitsF) const;
	void render();

private:
	ShaderIF* shaderIF;
	Cylinder* trunk;
	Block* treeTop;

	float xmin, xmax, ymin, ymax, zmin, zmax;
	float kd[3];
};

#endif
