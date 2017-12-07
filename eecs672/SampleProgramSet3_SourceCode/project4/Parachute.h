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
	Parachute(ShaderIF* sIF, cryph::AffPoint corner, double length);
	virtual ~Parachute();

	// xyzLimits: {mcXmin, mcXmax, mcYmin, mcYmax, mcZmin, mcZmax}
	void getMCBoundingBox(double* xyzLimitsF) const;
	bool handleCommand(unsigned char anASCIIChar, double ldsX, double ldsY);
	void render();

private:
	BasicShape* parachute[3];
	BasicShapeRenderer* parachuteR[3];

	double xyz[6];
	void defineParahute();
};

#endif
