// Ground.h

#ifndef GROUND_H
#define GROUND_H

#include "ModelView.h"
#include "ShaderIF.h"
#include "Block.h"

typedef float vec3[3];

class Ground : public ModelView
{
public:
	// As before: you will likely want to add parameters to the constructor
	Ground(ShaderIF* sIF, float xMin, float yMin, float zMin,
		     float lenX, float lenY, float lenZ, vec3 color);
	virtual ~Ground();

	// xyzLimits: {mcXmin, mcXmax, mcYmin, mcYmax, mcZmin, mcZmax}
	void getMCBoundingBox(double* xyzLimitsF) const;
	void render();

private:
	ShaderIF* shaderIF;
	Block* groundBlock;

	float xmin, xmax, ymin, ymax, zmin, zmax;
	float kd[3];
};

#endif
