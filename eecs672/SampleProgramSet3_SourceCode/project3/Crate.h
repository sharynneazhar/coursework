// Crate.h

#ifndef CRATE_H
#define CRATE_H

#include "Block.h"
// #include "Parachute.h"

class Crate : public SceneElement
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
	Block* crateTop;
	Block* crateBase;
	// Parachute* parachute;

	bool inAir;
	float xmin, xmax, ymin, ymax, zmin, zmax;
};

#endif
