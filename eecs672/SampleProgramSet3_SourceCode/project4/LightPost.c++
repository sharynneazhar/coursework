// LightPost.c++

#include "LightPost.h"

PhongMaterial lightPhong(0.312, 0.312, 0.312, 0.5, 0.5, 0.5, 1, 1);

LightPost::LightPost(ShaderIF* sIF, PhongMaterial&matl, float cx, float cy, float cz,
	float lx, float ly, float lz, int direction) : SceneElement(sIF, matl)
{
	xyz[0] = cx; xyz[1] = cx + lx;
	xyz[2] = cy; xyz[3] = cy + ly;
	xyz[4] = cz; xyz[5] = cz + lz;

	leg = new Block(sIF, lightPhong, cx, cy, cz, lz, ly * 1.0, lz);

	// Determine if light post lamp & bulb should face right or left
	cryph::AffPoint bulbCenter(0,0,0);
	if (direction == 0) {
		light = new Block(sIF, matl, cx - (1.75 * lx), cy + (ly - lx), cz - (lz / 2), lx * 2, ly / 4, lz * 2);
		bulbCenter = cryph::AffPoint(cx - (0.75 * lx), cy + ly - 0.5, cz + 0.17);
	} else {
		light = new Block(sIF, matl, cx + (0.75 * lx), cy + (ly - lx), cz - (lz / 2), lx * 2, ly / 4, lz * 2);
	        bulbCenter = cryph::AffPoint(cx + (1.75 * lx), cy + ly - 0.5, cz + 0.17);
	}

	lightBulb = new LightBulb(sIF, bulbCenter);
}

LightPost::~LightPost() {
	delete leg;
	delete light;
	delete lightBulb;
}

bool LightPost::handleCommand(unsigned char anASCIIChar, double ldsX, double ldsY) {
	// Building does not look for events; just hand off to inherited handleCommand.
	return this->SceneElement::handleCommand(anASCIIChar, ldsX, ldsY);
}

// xyzLimits: {mcXmin, mcXmax, mcYmin, mcYmax, mcZmin, mcZmax}
void LightPost::getMCBoundingBox(double* xyzLimits) const {
	for (int i = 0 ; i < 6; i++)
		xyzLimits[i] = xyz[i];
}

void LightPost::render()
{
	ProjController* c = dynamic_cast<ProjController*>(Controller::getCurrentController());
	if (c->drawingOpaque()) {
		return;
	}

	leg->render();
	lightBulb->render();

	glUniform1f(shaderIF->ppuLoc("shininess"), 100);
	glUniform1f(shaderIF->ppuLoc("alpha"), 0.6);
	light->render();
}
