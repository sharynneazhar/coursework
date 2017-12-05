// Building.h

#ifndef BUILDING_H
#define BUILDING_H

#include "SceneElement.h"
#include "AffPoint.h"
#include "AffVector.h"
#include "BasicShapeRenderer.h"
#include "Door.h"

class Building : public SceneElement
{
public:
	Building(ShaderIF* sIF, cryph::AffPoint corner);
	virtual ~Building();

	// xyzLimits: {mcXmin, mcXmax, mcYmin, mcYmax, mcZmin, mcZmax}
	void getMCBoundingBox(double* xyzLimitsF) const;
	void render();

private:
	BasicShape* building[2];
	BasicShapeRenderer* buildingR[2];
	Door* door;

	double xyz[6];
	void drawBuilding();

};

#endif
