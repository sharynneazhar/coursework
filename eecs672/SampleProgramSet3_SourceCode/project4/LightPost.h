// LightPost.h

#ifndef LIGHTPOST_H
#define LIGHTPOST_H

#include "ProjController.h"
#include "SceneElement.h"
#include "ShaderIF.h"
#include "Block.h"
#include "LightBulb.h"

class LightPost : public SceneElement
{
public:
	LightPost(ShaderIF* sIF, PhongMaterial&matl,
						float cx, float cy, float cz,
						float lx, float ly, float lz,
					  int direction);

	virtual ~LightPost();

	void getMCBoundingBox(double* xyzLimitsF) const;
	bool handleCommand(unsigned char anASCIIChar, double ldsX, double ldsY);
	void render();

private:
	double xyz[6];
	Block* leg;
	Block* light;
	LightBulb* lightBulb;
};

#endif
