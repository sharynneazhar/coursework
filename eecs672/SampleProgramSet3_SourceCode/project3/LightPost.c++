// LightPost.c++

#include "LightPost.h"

LightPost::LightPost(ShaderIF* sIF, PhongMaterial&matl, float cx, float cy, float cz, float lx, float ly, float lz, int direction) : SceneElement(sIF, matl)
{
	PhongMaterial lightPhong(0.32, 0.2, 0.039, 0.32, 0.2, 0.039, 0.32, 0.2, 0.039, 1, 1);

	xmin = cx; xmax = cx + lx;
	ymin = cy; ymax = cy + ly;
	zmin = cz; zmax = cz + lz;

	leg = new Block(sIF, lightPhong, cx, cy, cz, lz, ly * 1.0, lz);

	if (direction == 0) // left face
		light = new Block(sIF, matl, cx - (1.75 * lx), cy + (ly - lx), cz, lx * 2, ly / 4, lz * 2);
	else // right face
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
