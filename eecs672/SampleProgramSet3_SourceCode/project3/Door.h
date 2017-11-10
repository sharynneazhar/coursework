// Door.h

#ifndef DOOR_H
#define DOOR_H

#include "SceneElement.h"
#include "AffPoint.h"
#include "AffVector.h"
#include "BasicShapeRenderer.h"

class Door : public SceneElement
{
public:
	Door(ShaderIF* sIF, cryph::AffPoint corner, cryph::AffVector u, double offset);
	virtual ~Door();

	// xyzLimits: {mcXmin, mcXmax, mcYmin, mcYmax, mcZmin, mcZmax}
	void getMCBoundingBox(double* xyzLimitsF) const;
	void render();

private:
	BasicShape* door;
	BasicShapeRenderer* doorR;

	double xyz[6];
	void drawDoor();

};

#endif
