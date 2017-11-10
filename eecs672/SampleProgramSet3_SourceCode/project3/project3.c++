// project3.c++: Starter for EECS 672 Project 2

#include "GLFWController.h"
#include "Block.h"
#include "Crate.h"
#include "LightPost.h"
#include "Building.h"

void set3DViewingInformation(double xyz[6])
{
	// Tell class ModelView we initially want to see the whole scene:
	ModelView::setMCRegionOfInterest(xyz);

	// Determine a reasonable initial view:
	// (i) Find the maximum of the three MC deltas
	double maxDelta = xyz[1] - xyz[0];
	double delta = xyz[3] - xyz[2];
	if (delta > maxDelta)
		maxDelta = delta;
	delta = xyz[5] - xyz[4];
	if (delta > maxDelta)
		maxDelta = delta;

	// (ii) Determine the center of the created scene:
	double xmid = 0.5 * (xyz[0] + xyz[1]);
	double ymid = 0.5 * (xyz[2] + xyz[3]);
	double zmid = 0.5 * (xyz[4] + xyz[5]);

	// Create the line of sight through the center of the scene:
	// 1) Make the center of attention be the center of the bounding box.
	cryph::AffPoint center(xmid, ymid, zmid);

	// 2) Move the eye away along some direction - here (0,0,1) - so that the
	//    distance between the eye and the center is (2 * max scene dimension).
	cryph::AffVector dir(0.0, 0.15, 1.0);
	dir.normalize();

	double distEyeCenter = 2.0 * maxDelta;
	cryph::AffPoint eye = center + distEyeCenter * dir;

	// 3) Set the "up" direction vector to orient the 3D view
	cryph::AffVector up = cryph::AffVector::yu;

	// Notify the ModelView of our MC->EC viewing requests:
	ModelView::setEyeCenterUp(eye, center, up);

	// COMMON HEURISTIC FOR ESTABLISHING THE PROJECTION TRANSFORMATION:
	// Place the projection plane (ECz = ecZpp) roughly at the front of scene
	// and set eye coordinate ecZmin/ecZmax clipping planes relative to it.
	// IMPORTANT NOTE: For perspective projections, the following must hold:
	// 1) ecZpp < 0
	// 2) ecZmin < ecZmax < 0
	// For non-perspective projections, it is only necessary that ecZmin < ecZmax.
	double ecZpp = -(distEyeCenter - 0.5 * maxDelta);
	double ecZmin = ecZpp - maxDelta;
	double ecZmax = ecZpp + 0.5 * maxDelta;

	ModelView::setProjection(PERSPECTIVE);
	ModelView::setProjectionPlaneZ(ecZpp);
	ModelView::setECZminZmax(ecZmin, ecZmax);
}

int main(int argc, char* argv[])
{
	GLFWController c("PUBG Disco?", MVC_USE_DEPTH_BIT);
	c.reportVersions(std::cout);

	ShaderIF* sIF = new ShaderIF("shaders/basic.vsh", "shaders/phong.fsh");

	// Draw the crates
	cryph::AffVector u(0.0, 1.0, 0.0);
	c.addModel(new Crate(sIF, cryph::AffPoint(8.0, 0.5, 10.0), u, 2.0, false));
	c.addModel(new Crate(sIF, cryph::AffPoint(4.0, 10.0, 10.0), u, 1.15, true));
	c.addModel(new Crate(sIF, cryph::AffPoint(20.0, 8.0, 0.0), u, 0.75, true));

	// Draw the building
	c.addModel(new Building(sIF, cryph::AffPoint(18.0, 0.5, 15.0), cryph::AffVector(0.0, 1.0, 0.0)));

	// Draw the lightposts
	PhongMaterial blueLight(0.0, 0.0, 0.7, 1, 1);
	PhongMaterial purpleLight(1.0, 0.2, 1.0, 1, 1);
	c.addModel(new LightPost(sIF, purpleLight, 23.0, 0.0, 23.0, 0.5, 5.0, 0.5, 0));
	c.addModel(new LightPost(sIF, blueLight, 12.0, 0.0, 23.0, 0.5, 5.0, 0.5, 1));

	// Draw the ground
	PhongMaterial groundPhong(0.685, 0.90, 0.4, 0.6, 0.6);
	c.addModel(new Block(sIF, groundPhong, 0.0, 0.0, 0.0, 25.0, 0.1, 25.0));

	// glClearColor(1.0, 1.0, 1.0, 0.0);
	glClearColor(0.0, 0.0, 0.0, 0.0);

	double xyz[6];
	c.getOverallMCBoundingBox(xyz);
	set3DViewingInformation(xyz);

	c.run();

	delete sIF;

	return 0;
}
