// ModelView.c++ - a basic combined Model and View for OpenGL

#include <iostream>
#include <cmath>

#include "ModelView.h"
#include "Controller.h"

double ModelView::mcRegionOfInterest[6] = { -1.0, 1.0, -1.0, 1.0, -1.0, 1.0 };
bool ModelView::aspectRatioPreservationEnabled = true;

ModelView::ModelView(ShaderIF* sIF, cryph::AffPoint centerIn,
	cryph::AffVector u, cryph::AffVector v, double radiusIn) :
		shaderIF(sIF), nVerticesAroundPerimeter(100),
		center(centerIn), radius(radiusIn)
{
	createColoredCircle(center, u, v, radius);
}

ModelView::~ModelView()
{
	glDeleteBuffers(2, vbo);
	glDeleteVertexArrays(1, vao);
}

void ModelView::compute2DScaleTrans(float* scaleTransF) // CLASS METHOD
{
	double xmin = mcRegionOfInterest[0];
	double xmax = mcRegionOfInterest[1];
	double ymin = mcRegionOfInterest[2];
	double ymax = mcRegionOfInterest[3];

	if (aspectRatioPreservationEnabled)
	{
		// preserve aspect ratio. Make "region of interest" wider or taller to
		// match the Controller's viewport aspect ratio.
		double vAR = Controller::getCurrentController()->getViewportAspectRatio();
		matchAspectRatio(xmin, xmax, ymin, ymax, vAR);
	}

	double scaleTrans[4];
	linearMap(xmin, xmax, -1.0, 1.0, scaleTrans[0], scaleTrans[1]);
	linearMap(ymin, ymax, -1.0, 1.0, scaleTrans[2], scaleTrans[3]);
	for (int i=0 ; i<4 ; i++)
		scaleTransF[i] = static_cast<float>(scaleTrans[i]);
}

void ModelView::createColoredCircle(cryph::AffPoint center,
	cryph::AffVector u, cryph::AffVector v, double radius)
{
	typedef float vec3[3];

	// Make sure u and v are unit vectors:
	u.normalize();
	v.normalize();

	// define vertex colors and positions
	vec3* vertexColors = new vec3[nVerticesAroundPerimeter+1]; // "+1" is for center
	vec3* vertexPositions = new vec3[nVerticesAroundPerimeter+1];

	center.aCoords(vertexPositions, 0); // circle center
	// colored white:
	vertexColors[0][0] = vertexColors[0][1] = vertexColors[0][2] = 1.0;
	double theta = 0.0, dTheta = 2.0*M_PI/(nVerticesAroundPerimeter-1);
	double rgb[3];
	for (int i=1 ; i<nVerticesAroundPerimeter ; i++)
	{
		cryph::AffPoint p = center + radius * (cos(theta)*u + sin(theta)*v);
		p.aCoords(vertexPositions, i);
		hsv2rgb(theta*180.0/M_PI, 1.0, 1.0, rgb);
		for (int j=0 ; j<3 ; j++)
			vertexColors[i][j] = static_cast<float>(rgb[j]);
		theta += dTheta;
	}
	// last point has same coordinates and colors as first. (First starts
	// at [1] since center is in positon [0].)
	for (int i=0 ; i<3 ; i++)
	{
		vertexPositions[nVerticesAroundPerimeter][i] = vertexPositions[1][i];
		vertexColors[nVerticesAroundPerimeter][i] = vertexColors[1][i];
	}

	// ***********************************
	// EXERCISE: create VAO and VBOs here.
	// ***********************************
	vao[0] = 0; // delete this line and the following "cout" when completing the exercise
	std::cout <<
		"ModelView::createColoredCircle: code has been left as an exercise.\n" <<
	    "You must complete it on order to see the result of this program.\n";

	// glBufferData copies data to server, so:
	delete [] vertexColors;
	delete [] vertexPositions;
}

// xyzLimits: {mcXmin, mcXmax, mcYmin, mcYmax, mcZmin, mcZmax}
void ModelView::getMCBoundingBox(double* xyzLimits) const
{
	xyzLimits[0] = center.x - radius; xyzLimits[1] = center.x + radius;
	xyzLimits[2] = center.y - radius; xyzLimits[3] = center.y + radius;
	xyzLimits[4] = -1.0; xyzLimits[5] = 1.0; // (zmin, zmax) (really 0..0)
}

bool ModelView::handleCommand(unsigned char anASCIIChar, double ldsX, double ldsY)
{
	return true;
}

void ModelView::hsv2rgb( // CLASS METHOD
			 // given the following (0<=hue<=360); saturation & value in 0..1 range:
			 double hue, double saturation, double value,
			 // compute the following in a 0..1 range:
			 double rgb[])
// Algorithm taken from: www.easyrgb.com
{
	if (saturation == 0 )  //HSV values = 0 .. 1
		rgb[0] = rgb[1] = rgb[2] = value;
	else
	{
		double var_h = hue / 60.0;
		if (var_h == 6.0)
			var_h = 0.0;      //H must be < 360
		int var_i = static_cast<int>(var_h);	//Or ... var_i = floor( var_h )
		double var_1 = value * ( 1.0 - saturation );
		double var_2 = value * ( 1.0 - saturation * ( var_h - var_i ) );
		double var_3 = value * ( 1.0 - saturation * ( 1.0 - ( var_h - var_i ) ) );
		
		switch (var_i)
		{
			case 0: rgb[0] = value; rgb[1] = var_3; rgb[2] = var_1; break;
			case 1: rgb[0] = var_2; rgb[1] = value; rgb[2] = var_1; break;
			case 2: rgb[0] = var_1; rgb[1] = value; rgb[2] = var_3; break;
			case 3: rgb[0] = var_1; rgb[1] = var_2; rgb[2] = value; break;
			case 4: rgb[0] = var_3; rgb[1] = var_1; rgb[2] = value; break;
			default:rgb[0] = value; rgb[1] = var_1; rgb[2] = var_2;
		}
	}
}

// linearMap determines the scale and translate parameters needed in
// order to map a value, f (fromMin <= f <= fromMax) to its corresponding
// value, t (toMin <= t <= toMax). Specifically: t = scale*f + trans.
void ModelView::linearMap(double fromMin, double fromMax, double toMin, double toMax,
						  double& scale, double& trans) // CLASS METHOD
{
	scale = (toMax - toMin) / (fromMax - fromMin);
	trans = toMin - scale*fromMin;
}

void ModelView::matchAspectRatio(double& xmin, double& xmax,
        double& ymin, double& ymax, double vAR)
{
	double wHeight = ymax - ymin;
	double wWidth = xmax - xmin;
	double wAR = wHeight / wWidth;
	if (wAR > vAR)
	{
		// make window wider
		wWidth = wHeight / vAR;
		double xmid = 0.5 * (xmin + xmax);
		xmin = xmid - 0.5*wWidth;
		xmax = xmid + 0.5*wWidth;
	}
	else
	{
		// make window taller
		wHeight = wWidth * vAR;
		double ymid = 0.5 * (ymin + ymax);
		ymin = ymid - 0.5*wHeight;
		ymax = ymid + 0.5*wHeight;
	}
}

void ModelView::render() const
{
	// save the current GLSL program in use
	GLint pgm;
	glGetIntegerv(GL_CURRENT_PROGRAM, &pgm);
	// draw the triangles using our vertex and fragment shaders
	glUseProgram(shaderIF->getShaderPgmID());

	// define the mapping from MC to -1..+1 Logical Device Space:
	float scaleTrans[4];
	compute2DScaleTrans(scaleTrans);
	glUniform4fv(shaderIF->ppuLoc("scaleTrans"), 1, scaleTrans);

	glBindVertexArray(vao[0]);
	glDrawArrays(GL_TRIANGLE_FAN, 0, nVerticesAroundPerimeter+1); // offset: 0
	// restore the previous program
	glUseProgram(pgm);
}

void ModelView::setMCRegionOfInterest(double xyz[6])
{
	for (int i=0 ; i<6 ; i++)
		mcRegionOfInterest[i] = xyz[i];
}
