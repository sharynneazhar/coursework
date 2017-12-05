// Ground.h

#ifndef GROUND_H
#define GROUND_H

#include "SceneElement.h"
#include "ShaderIF.h"
#include "ImageReader.h"

class Ground : public SceneElement
{
public:
	Ground(ShaderIF* sIF, PhongMaterial&matl, float width, float depth);
	virtual ~Ground();

	// xyzLimits: {mcXmin, mcXmax, mcYmin, mcYmax, mcZmin, mcZmax}
	void getMCBoundingBox(double* xyzLimitsF) const;
	bool handleCommand(unsigned char anASCIIChar, double ldsX, double ldsY);
	void render();

private:
	GLuint vao[1];
	GLuint vbo[1];
	GLuint ebo[3];

	static GLuint indexList[3][4];
	float xyz[6];

	void defineInitialGeometry();
	void establishTextureEnvironment();
	GLuint defineTexture(const char* texImageSource);

};

#endif
