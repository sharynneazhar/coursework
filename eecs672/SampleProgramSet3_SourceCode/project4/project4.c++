// project3.c++: Starter for EECS 672 Project 2

#include <iostream>
#include <stdlib.h>
#include <time.h>

#include "ProjController.h"
#include "Block.h"
#include "Ground.h"
#include "Crate.h"
#include "Parachute.h"
#include "LightPost.h"
#include "Building.h"
#include "Tree.h"

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
	cryph::AffPoint center(xmid, ymid + 2, zmid);

	// 2) Move the eye away along some direction - here (0,0,1) - so that the
	//    distance between the eye and the center is (2 * max scene dimension).
	cryph::AffVector dir(0.0, 0.15, 1.0);
	dir.normalize();

	double distEyeCenter = 2.0 * maxDelta;
	cryph::AffPoint eye = center + distEyeCenter * dir;

	// 3) Set the "up" direction vector to orient the 3D view
	cryph::AffVector up(0.0, 1.0, 0.0);

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
	srand(time(NULL)); // initialize random seed

	ProjController c("PUBG Cargo Air Drops", MVC_USE_DEPTH_BIT);
	c.reportVersions(std::cout);

	ShaderIF* sIF = new ShaderIF("shaders/basic.vsh", "shaders/phong.fsh");
	cryph::AffVector u(0.0, 1.0, 0.0);

	// Draw the crates
	// 1. A crate on the ground
	// 2. Up to 5 different parachuting crates randomly placed
	c.addModel(new Crate(sIF, cryph::AffPoint(8.0, 0.0, 10.0), 1.75));
	for (int i = 0; i < rand() % 5 + 2; i++) {
		double xPos = (20.0 - 1.0) * ((double) rand() / (double) RAND_MAX) + 4.0;
		double yPos = (15.0 - 10.0) * ((double) rand() / (double) RAND_MAX) + 10.0;
		double zPos = (10.0 - 1.0) * ((double) rand() / (double) RAND_MAX) + 4.0;
		double size = (1.0 - 0.5) * ((double) rand() / (double) RAND_MAX) + 0.5;
		c.addModel(new Crate(sIF, cryph::AffPoint(xPos, yPos, zPos), 1.0));
		c.addModel(new Parachute(sIF, cryph::AffPoint(xPos, yPos, zPos), 1.0));
	}

	// Draw the building
	c.addModel(new Building(sIF, cryph::AffPoint(18.0, 0.0, 15.0)));

	// Draw the lightposts
	PhongMaterial blueLight(0.25, 0.5, 1.0, 0.1, 0.396, 0.097254, 12, 0.5);
	PhongMaterial purpleLight(1.0, 0.2, 1.0, 0.1, 0.396, 0.097254, 12, 0.5);
	c.addModel(new LightPost(sIF, purpleLight, 23.0, 0.0, 23.0, 0.45, 5.0, 0.45, 0));
	c.addModel(new LightPost(sIF, blueLight, 2.0, 0.0, 5.0, 0.45, 5.0, 0.45, 1));

	// Draw the trees
	// Up to 10 trees randomly placed
	for (int i = 0; i < rand() % 15 + 4; i++) {
		double xPos = (13.0 - 1.0) * ((double) rand() / (double) RAND_MAX) + 1.0;
		double zPos = (23.0 - 2.0) * ((double) rand() / (double) RAND_MAX) + 2.0;

		// Check if the tree will be on the crate, move it
		bool isOnCrate = (xPos >= 7.0 && xPos <= 13.0) && (zPos >= 7.0 && zPos <= 13.0);
		if (isOnCrate)
			c.addModel(new Tree(sIF, cryph::AffPoint((xPos + 2.2), -0.2, zPos)));
		else
			c.addModel(new Tree(sIF, cryph::AffPoint(xPos, -0.2, zPos)));
	}

	// Draw the ground
	c.addModel(new Ground(sIF, 25.0, 25.0));

	// Specify background color
	glClearColor(0.1, 0.1, 0.1, 1.0);

	double xyz[6];
	c.getOverallMCBoundingBox(xyz);
	set3DViewingInformation(xyz);

	c.run();

	delete sIF;

	return 0;
}
