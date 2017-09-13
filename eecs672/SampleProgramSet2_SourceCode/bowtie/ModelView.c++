// ModelView.c++ - a basic combined Model and View for OpenGL

#include <iostream>
#include <cmath>

#include "ModelView.h"

#include "GLFWController.h"
double delayInSeconds = 0.080;

#include "CFont.h"
#include "CGLString.h"

double ModelView::mcRegionOfInterest[6] = { -1.0, 1.0, -1.0, 1.0, -1.0, 1.0 };
bool ModelView::aspectRatioPreservationEnabled = true;

ModelView::ModelView(ShaderIF* sIF, cryph::AffPoint centerIn,
	cryph::AffVector u, cryph::AffVector v, double radiusIn, double a,
	std::string font) : shaderIF(sIF),
		nVerticesAroundPerimeter(100), center(centerIn), radius(radiusIn),
		labelString(nullptr), labelFont(nullptr),
		lastAnimationUpdate(0.0),
		animating(false), labelAngle(-0.5*M_PI)
{
	createColoredBowTie(center, u, v, radius, a);

	labelFont = CFont::getFont(font);
	if (labelFont != nullptr)
	{
		// create the label
		labelString = new CGLString("Look at my colorful bow tie!!",
		                   labelFont, 2); // "2" ==> all coordinates are 2D
		// how long do I want it to be in MC? (This can be changed at any time,
		// e.g., during animation). Let's make it 60% of the diameter.
		double stringWidth = 1.2 * radius;
		labelString->setStringDimensions(stringWidth, 0.0);
		// where does it start? (This can be changed at any time, e.g., during animation)
		// We'll center it in x, and have it 25% of the way from the bottom to the top.
		double ox = center.x - 0.6*radius, oy = center.y - 0.75*radius;
		labelString->setStringOrigin(ox, oy, 0.0);

		// extra information needed for our animation
		labelRotationRadius = sqrt(ox*ox + oy*oy);
		labelAngle = atan2(oy, ox);
	}
}

ModelView::~ModelView()
{
	glDeleteBuffers(2, vbo);
	glDeleteVertexArrays(1, vao);
	if (labelFont != nullptr)
		delete labelFont;
	if (labelString != nullptr)
		delete labelString;
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

void ModelView::createColoredBowTie(cryph::AffPoint center,
	cryph::AffVector u, cryph::AffVector v, double radius, double a)
{
	typedef float vec3[3];

	// Make sure u and v are unit vectors:
	u.normalize();
	v.normalize();

	// define vertex colors and positions

	// **********************************************************************
	// EXERCISE: Modify the circle code to do the pinch as mentioned ********
	//           on the web page and discussed in class.             ********
	// **********************************************************************
	std::cout <<
		"ModelView::createColoredBowTie (1): code has been left as an exercise.\n" <<
	    "You must complete it on order to see the result of this program.\n";

	// ***********************************
	// EXERCISE: create VAO and VBOs here.
	// ***********************************
	vao[0] = 0; // delete this line and the following "cout" when completing the exercise
	std::cout <<
		"ModelView::createColoredBowTie (2): code has been left as an exercise.\n" <<
	    "You must complete it on order to see the result of this program.\n";
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
	// If the character is an 'a' - toggle animation - I assume the message is
	// for me. I will handle it and tell the Controller to stop.
	if (anASCIIChar == 'a')
	{
		animating = !animating;
		if (animating)
		{
			GLFWController* glfwC =
				dynamic_cast<GLFWController*>(Controller::getCurrentController());
			if (glfwC != nullptr)
			{
				glfwC->setRunWaitsForAnEvent(false);
				maybeRotateLabel();
			}
		}
		else
		{
			GLFWController* glfwC =
				dynamic_cast<GLFWController*>(Controller::getCurrentController());
			if (glfwC != nullptr)
				glfwC->setRunWaitsForAnEvent(true);
		}
		return false; // tell Controller to stop.
	}
	return true; // Not an animation toggle; tell Controller to keep trying
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

void ModelView::maybeRotateLabel()
{
	if (animating)
	{
		double time = glfwGetTime();
		if (time > lastAnimationUpdate+delayInSeconds)
		{
			rotateLabel();
			lastAnimationUpdate = time;
		}
	}
}

void ModelView::render()
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
	glUniform1i(shaderIF->ppuLoc("renderingFontString"), 0); // signal not rendering strings

	// render the main geometry (our bowtie)
	// bind the vertex array as usual, then draw the bowtie
	glBindVertexArray(vao[0]);
	glDrawArrays(GL_TRIANGLE_FAN, 0, nVerticesAroundPerimeter+1); // offset: 0

	// then draw the string
	renderString();

	// restore the previous program
	glUseProgram(pgm);

	maybeRotateLabel();
}

void ModelView::renderString() const
{
	if (labelString == nullptr)
		return;

	// set the font color
	float fColor[] = { 0.0, 0.0, 1.0, 1.0 }; // color to be used to render the font
	glUniform4fv(shaderIF->ppuLoc("fontColor"), 1, fColor);
	// Tell the fragment shader that we are rendering a string so that it will know
	// to use the texture map created by CGLString:
	glUniform1i(shaderIF->ppuLoc("renderingFontString"), 1);

	// Now render the string:
	labelString->render(shaderIF->pvaLoc("mcPosition"), shaderIF->pvaLoc("texCoords"), shaderIF->ppuLoc("fontTextureMap"));

	// Done rendering the string:
	glUniform1i(shaderIF->ppuLoc("renderingFontString"), 0);
}

void ModelView::rotateLabel()
{
	labelAngle += 0.05;
	double r = labelRotationRadius;
	labelString->setStringOrigin(r * cos(labelAngle), r * sin(labelAngle), 0.0);
	// we could change things other than its position. For example, we could
	// "animate" its color; we could spin it around; make it bigger/smaller; etc.
}

void ModelView::setMCRegionOfInterest(double xyz[6])
{
	for (int i=0 ; i<6 ; i++)
		mcRegionOfInterest[i] = xyz[i];
}
