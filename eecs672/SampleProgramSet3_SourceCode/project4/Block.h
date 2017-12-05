// Block.h

#ifndef BLOCK_H
#define BLOCK_H

#include "SceneElement.h"
#include "ShaderIF.h"

class Block : public SceneElement
{
public:
	Block(ShaderIF* sIF, PhongMaterial&matl,
				float cx, float cy, float cz,
				float lx, float ly, float lz);
	virtual ~Block();

	// xyzLimits: {mcXmin, mcXmax, mcYmin, mcYmax, mcZmin, mcZmax}
	void getMCBoundingBox(double* xyzLimitsF) const;
	bool handleCommand(unsigned char anASCIIChar, double ldsX, double ldsY);
	void render();

private:
	GLuint vao[1];
	GLuint vbo[1];
	GLuint ebo[3];

	static GLuint indexList[3][4];

	float xmin, xmax, ymin, ymax, zmin, zmax;

	void drawBlock();
};

#endif
