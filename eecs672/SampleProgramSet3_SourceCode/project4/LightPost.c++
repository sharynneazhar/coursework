// LightPost.c++

#include "LightPost.h"

PhongMaterial lightPhong(0.312, 0.312, 0.312, 0.5, 0.5, 0.5, 1, 1);

LightPost::LightPost(ShaderIF* sIF, PhongMaterial&matl, float cx, float cy, float cz,
	float lx, float ly, float lz, int direction) : SceneElement(sIF, matl)
{
	xmin = cx; xmax = cx + lx;
	ymin = cy; ymax = cy + ly;
	zmin = cz; zmax = cz + lz;

	leg = new Block(sIF, lightPhong, cx, cy, cz, lz, ly * 1.0, lz);

	// Determine if light post lamp should face right or left
	if (direction == 0)
		light = new Block(sIF, matl, cx - (1.75 * lx), cy + (ly - lx), cz, lx * 2, ly / 4, lz * 2);
	else
		light = new Block(sIF, matl, cx + (0.75 * lx), cy + (ly - lx), cz, lx * 2, ly / 4, lz * 2);
}

LightPost::~LightPost()
{
	delete leg;
	delete light;
}

// xyzLimits: {mcXmin, mcXmax, mcYmin, mcYmax, mcZmin, mcZmax}
void LightPost::getMCBoundingBox(double* xyzLimits) const
{
	xyzLimits[0] = xmin;
	xyzLimits[1] = xmax;
	xyzLimits[2] = ymin;
	xyzLimits[3] = ymax;
	xyzLimits[4] = zmin;
	xyzLimits[5] = zmax;
}

void LightPost::render()
{
	leg->render();
	light->render();
}
