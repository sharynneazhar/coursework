// Crate.h

#ifndef CRATE_H
#define CRATE_H

#include "ModelView.h"
#include "ShaderIF.h"
#include "Block.h"
#include "Parachute.h"

typedef float vec3[3];

class Crate : public ModelView
{
public:
	// As before: you will likely want to add parameters to the constructor
	Crate(ShaderIF* sIF, float xMin, float yMin, float zMin,
		    float lenX, float lenY, float lenZ, bool inAirIn);
	virtual ~Crate();

	// xyzLimits: {mcXmin, mcXmax, mcYmin, mcYmax, mcZmin, mcZmax}
	void getMCBoundingBox(double* xyzLimitsF) const;
	void render();

private:
	ShaderIF* shaderIF;
	Block* crateTop;
	Block* crateBase;
	Parachute* parachute;

	float xmin, xmax, ymin, ymax, zmin, zmax;
	float kd[3];
	bool inAir;
};

#endif
