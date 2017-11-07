// Parachute.h

#ifndef PARACHUTE_H
#define PARACHUTE_H

#include "ModelView.h"
#include "ShaderIF.h"

typedef float vec3[3];

class Parachute : public ModelView
{
public:
	// As before: you will likely want to add parameters to the constructor
	Parachute(ShaderIF* sIF, cryph::AffPoint bottomIn, double radiusIn, double heightIn);
	virtual ~Parachute();

	// xyzLimits: {mcXmin, mcXmax, mcYmin, mcYmax, mcZmin, mcZmax}
	void getMCBoundingBox(double* xyzLimitsF) const;
	void render();

private:
	ShaderIF* shaderIF;
	GLuint vao[1];
	GLuint vbo[2];

	const int radius;

	float kd[3];
	float xyz[6];

	cryph::AffPoint bottom;
	cryph::AffPoint top;
	cryph::AffVector axis;

	void defineParachute();
};

#endif
