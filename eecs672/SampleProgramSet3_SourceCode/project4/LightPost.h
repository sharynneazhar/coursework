// LightPost.h

#ifndef LIGHTPOST_H
#define LIGHTPOST_H

#include "SceneElement.h"
#include "ShaderIF.h"
#include "Block.h"

class LightPost : public SceneElement
{
public:
	LightPost(ShaderIF* sIF, PhongMaterial&matl,
						float cx, float cy, float cz,
						float lx, float ly, float lz,
					  int direction);

	virtual ~LightPost();

	void getMCBoundingBox(double* xyzLimitsF) const;
	void render();

private:
	float xmin, xmax, ymin, ymax, zmin, zmax;
	Block* leg;
	Block* light;
};

#endif
