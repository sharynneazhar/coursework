// Parachute.h

#ifndef PARACHUTE_H
#define PARACHUTE_H

#include "SceneElement.h"
#include "AffPoint.h"
#include "AffVector.h"
#include "BasicShapeRenderer.h"

class Parachute : public SceneElement
{
public:
	Parachute(ShaderIF* sIF, cryph::AffPoint corner, cryph::AffVector u, double radius);
	virtual ~Parachute();

	// xyzLimits: {mcXmin, mcXmax, mcYmin, mcYmax, mcZmin, mcZmax}
	void getMCBoundingBox(double* xyzLimitsF) const;
	void render();

private:
	BasicShape* parachute;
	BasicShapeRenderer* parachuteR;

	double xyz[6];
	void drawParachute();

};

#endif
