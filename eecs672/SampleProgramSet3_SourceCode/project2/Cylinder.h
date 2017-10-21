// Cylinder.h

#ifndef CYLINDER_H
#define CYLINDER_H

#include "ModelView.h"
#include "ShaderIF.h"

typedef float vec3[3];

class Cylinder : public ModelView
{
public:
	Cylinder(ShaderIF* sIF, float xPos, float yPos, float zPos,
					 float xRotIn, float yRotIn, float zRotIn,
					 float rad, float len, float adj, vec3 color);
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
	bool displayCylEdges, displayCylFill;

	void defineCylinder(float xPos, float yPos, float zPos, float xRotIn, float yRotIn, float zRotIn, float rad, float len, float adj);
};

#endif
