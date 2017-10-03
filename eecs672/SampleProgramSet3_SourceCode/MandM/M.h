// M.h

#ifndef MANDM_H
#define MANDM_H

#include <GL/gl.h>

#include "ModelView.h"
#include "ShaderIF.h"

class M : public ModelView
{
public:
	M(ShaderIF* sIF, double dXYZ[], float colorIn[]);
	virtual ~M();

	// xyzLimits: {mcXmin, mcXmax, mcYmin, mcYmax, mcZmin, mcZmax}
	void getMCBoundingBox(double* xyzLimits) const;
	bool handleCommand(unsigned char anASCIIChar, double ldsX, double ldsY);
	void render();
private:
	ShaderIF* shaderIF;
	GLuint vao[1];
	GLuint vbo[1];
	double xyzMinMax[6];
	float kd[3];

	void defineM(double dXYZ[]);
	void renderM();
};

#endif
