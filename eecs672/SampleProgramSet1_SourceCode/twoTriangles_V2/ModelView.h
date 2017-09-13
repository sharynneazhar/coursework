// ModelView.h - a basic combined Model and View for OpenGL

#ifndef MODELVIEW_H
#define MODELVIEW_H

#include "ShaderIF.h"

#include <string>

#ifdef __APPLE_CC__
#include "GLFW/glfw3.h"
#else
#include <GL/gl.h>
#endif

typedef float vec2[2];

class ModelView
{
public:
	ModelView(ShaderIF* sIF, vec2* triangleVertices);
	virtual ~ModelView();

	// xyzLimits: {mcXmin, mcXmax, mcYmin, mcYmax, mcZmin, mcZmax}
	void getMCBoundingBox(double* xyzLimits) const;
	bool handleCommand(unsigned char anASCIIChar, double ldsX, double ldsY);
	void render() const;

	static void setMCRegionOfInterest(double xyz[]);

private:
	GLuint vao[1]; // VAO for the triangle
	GLuint vbo[1]; // VBO for the triangle coordinates

	int colorMode; // flag to tell fragment shader how to assign colors
	double xmin, xmax, ymin, ymax; // track the limits of this instance

	ShaderIF* shaderIF;

	void deleteObject();
	void initModelGeometry(vec2* vertices);

	// Routines for computing parameters necessary to map from arbitrary
	// model coordinate ranges into OpenGL's -1..+1 Logical Device Space.
	// 1. linearMap determines the scale and translate parameters needed in
	//    order to map a value, f (fromMin <= f <= fromMax) to its corresponding
	//    value, t (toMin <= t <= toMax). Specifically: t = scale*f + trans.
	static void linearMap(double fromMin, double fromMax,
		double toMin, double toMax, double& scale, double& trans);
	// 2. compute2DScaleTrans uses the current model coordinate region of
	//    interest along with linearMap to determine how to map coordinates
	//    in the region of interest to their proper location in Logical Device
	//    Space.
	//    (Returns float[] because glUniform currently allows only float[].)
	static void compute2DScaleTrans(float* scaleTrans);

	// Current MC region of interest
	static double mcRegionOfInterest[6];
};

#endif
