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

#include "AffPoint.h"
#include "AffVector.h"

class CFont;
class CGLString;

class ModelView
{
public:
	ModelView(ShaderIF* sIF, cryph::AffPoint centerIn,
		cryph::AffVector u, cryph::AffVector v, double radiusIn, double a,
		std::string font);
	virtual ~ModelView();

	void getMCBoundingBox(double* xyzLimits) const; // xyz min/max
	bool handleCommand(unsigned char anASCIIChar, double ldsX, double ldsY);
	void render();

	// Viewing controls common to 2D and 3D:
	static void setAspectRatioPreservationEnabled(bool b)
		{ aspectRatioPreservationEnabled = b; }
	static void setMCRegionOfInterest(double xyz[6]);

private:
	ShaderIF* shaderIF;
	int nVerticesAroundPerimeter;
	GLuint vao[1];
	GLuint vbo[2];
	cryph::AffPoint center;
	double radius;

	// String/Font information for label
	CGLString* labelString;
	CFont* labelFont;
	// For label animation:
	double lastAnimationUpdate;
	void maybeRotateLabel();
	bool animating;
	double labelAngle, labelRotationRadius;
	void rotateLabel();
	// END: For label animation

	// Routines for computing parameters necessary to map from arbitrary
	// model coordinate ranges into OpenGL's -1..+1 Logical Device Space.
	// 1. linearMap determines the scale and translate parameters needed in
	//    order to map a value, f (fromMin <= f <= fromMax) to its corresponding
	//    value, t (toMin <= t <= toMax). Specifically: t = scale*f + trans.
	static void linearMap(double fromMin, double fromMax,
		double toMin, double toMax, double& scale, double& trans);
	// 2. matchAspectRatio modifies (xmin,xmax) OR (ymin,ymax) to center that
	//    region in an expanded window that matches the given aspect ratio.
	static void matchAspectRatio(double& xmin, double& xmax,
		double& ymin, double& ymax, double vAR);
	// 3. compute2DScaleTrans uses the current model coordinate region of
	//    interest - modified as necessary to preserve aspect ratios - and
	//    then passes the modified limits to linearMap to compute the scale
	//    and translation needed to map MC to LDS.
	//    (The scales and translationes are returned in float[] because
	//    glUniform currently allows only float[].)
	static void compute2DScaleTrans(float* scaleTrans);

	static double mcRegionOfInterest[6];
	static bool aspectRatioPreservationEnabled;

	// 'a' is a bow tie parameter; typically want: 0 < a < 1.
	void createColoredBowTie(cryph::AffPoint center,
		cryph::AffVector u, cryph::AffVector v, double radius, double a);
	void renderString() const;

	static void hsv2rgb(double hue, double saturation, double value,
						double rgb[]);
};

#endif
