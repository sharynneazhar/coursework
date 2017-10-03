// Cylinder.h

#ifndef CYLINDER_H
#define CYLINDER_H

#include <GL/gl.h>

#include "ModelView.h"
#include "ShaderIF.h"

class Cylinder : public ModelView
{
public:
	Cylinder(ShaderIF* sIF);
	virtual ~Cylinder();

	// xyzLimits: {mcXmin, mcXmax, mcYmin, mcYmax, mcZmin, mcZmax}
	void getMCBoundingBox(double* xyzLimits) const;
	bool handleCommand(unsigned char anASCIIChar, double ldsX, double ldsY);
	void render();
private:
	ShaderIF* shaderIF;
	GLuint vao[1];
	GLuint vbo[2]; // 0: coordinates; 1: normal vectors
	float kd[3];

	void defineCylinder();
};

#endif
