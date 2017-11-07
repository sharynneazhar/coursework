// project2.c++: Starter for EECS 672 Project 2

#include "GLFWController.h"
#include "Crate.h"
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
	cryph::AffPoint center(xmid, ymid, zmid);

	// 2) Move the eye away along some direction - here (0,0,1) - so that the
	//    distance between the eye and the center is (2 * max scene dimension).
	cryph::AffVector dir(0, 0.2, 0.8);
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
	GLFWController c("PUBG Air Drops", MVC_USE_DEPTH_BIT);
	c.reportVersions(std::cout);

	ShaderIF* sIF = new ShaderIF("shaders/basic.vsh", "shaders/phong.fsh");

	// Create your scene, adding things to the Controller....

	// Draw the ground
	vec3 groundColor = { 0.689, 0.80, 0.55 };
	PhongMaterial groundPhong(0.2125, 0.1275, 0.054, 0.714, 0.4284, 0.18144, 0.393548, 0.271906, 0.166721, 25.6, 1);
	c.addModel(new Block(sIF, groundPhong, -5.0, 0.0, 0.0, 15.0, 0.5, 12.5));

	// Draw the crates
	// c.addModel(new Crate(sIF, groundPhong, 3.0, 0.5, 4.2, 1.5, 1.5, 1.5, false));
	// c.addModel(new Crate(sIF, groundPhong, 7.0, 4.0, 1.2, 0.5, 0.5, 0.5, true));
	// c.addModel(new Crate(sIF, groundPhong, 0.0, 4.0, 1.2, 0.5, 0.5, 0.5, true));
	//
	// // Draw the trees
	// c.addModel(new Tree(sIF, groundPhong, -1.35, 0.0, 7.2));
	// c.addModel(new Tree(sIF, groundPhong, -1.5, 0.0, 3.5));
	// c.addModel(new Tree(sIF, groundPhong, -4.0, 0.0, 5.5));
	// c.addModel(new Tree(sIF, groundPhong, 7.5, 0.0, 4.2));
	// c.addModel(new Tree(sIF, groundPhong, 8.6, 0.0, 2.5));
	//
	// // Draw bushes
	// PhongMaterial bushPhong(0, 1, 0, 0, 1, 0, 0, 1, 0, 15, 1);
	// c.addModel(new Block(sIF, bushPhong, 8.2, 0.5, 8.3, 0.75, 0.75, 0.75));
	// c.addModel(new Block(sIF, bushPhong, 8.6, 0.5, 5.3, 0.75, 0.75, 0.75));
	// c.addModel(new Block(sIF, bushPhong, 7.6, 0.5, 6.3, 0.75, 0.75, 0.75));

	// Make background white
	glClearColor(1.0, 1.0, 1.0, 1.0);

	double xyz[6];
	c.getOverallMCBoundingBox(xyz);
	set3DViewingInformation(xyz);

	c.run();

	delete sIF;

	return 0;
}
