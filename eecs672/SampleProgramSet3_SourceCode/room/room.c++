// room.c++: An OpenGL program that creates a nice little room

#include <string.h>

#include "GLFWController.h"
#include "Barbell.h"
#include "Floor.h"
#include "Tetrahedron.h"
#include "Table.h"

void createRoom(Controller& c, ShaderIF* sIF)
{
	// units are roughly in feet
	cryph::AffPoint c1(-8.0, -5, 0);
	cryph::AffVector u1(1.0, -0.5, 0.0);
	double legHeight = 2.5, tableThick = 0.25;
	PhongMaterial tableMatl(0.8, 0.0, 0.0, 0.3, 0.3, 0.6, 15.0, 1.0);
	Table* t1 = new Table(sIF, tableMatl, c1, u1, legHeight, 0.3, 5.0, 3.5, tableThick);
	t1->setTextureImage("images/ku_seal_1024.png");
	c.addModel( t1 );

	cryph::AffPoint c2(1.0, 4, 0);
	cryph::AffVector u2(0.0, 1.0, 0.0);
	Table* t2 = new Table(sIF, tableMatl, c2, u2, legHeight, 0.3, 5.0, 3.5, tableThick);
	c.addModel( t2 );
	t2->setTextureImage("images/albert_500.png");

	cryph::AffPoint topTable1 = c1 + (legHeight+tableThick)*cryph::AffVector::zu;
	cryph::AffPoint p0 = topTable1 + cryph::AffVector(2.3, 1.4, 0.0);
	cryph::AffPoint p1 = topTable1 + cryph::AffVector(3.3, 1.8, 0.0);
	cryph::AffPoint p2 = topTable1 + cryph::AffVector(2.3, 2.5, 0.0);
	cryph::AffPoint p3 = topTable1 + cryph::AffVector(2.6, 1.9, 1.6);
	PhongMaterial tetMatl(1.0, 1.0, 0.0, 0.2, 0.2, 0.8, 20.0, 1.0);
	c.addModel( new Tetrahedron(sIF, tetMatl, p0, p1, p2, p3));

	PhongMaterial floorMatl(0.6, 0.8, 0.4, 0.3, 0.5, 0.4, 1.0, 1.0);
	c.addModel( new Floor(sIF, floorMatl, -10.0, 10.0, -10.0, 10.0) );

	PhongMaterial barbellMatl(0.1, 0.1, 0.1, 0.2, 0.5, 9.5, 55.0, 1.0);
	double barR = 0.25, bellR = 1.0;
	c.addModel( new Barbell(sIF, barbellMatl, cryph::AffPoint(-9.0, 7.0, bellR),
		cryph::AffPoint(-6.5, 9.5, bellR), barR, bellR) );
	c.addModel( new Barbell(sIF, barbellMatl, cryph::AffPoint(8.0, -7.0, bellR),
		cryph::AffPoint(8.0, -3.5, bellR), barR, bellR) );
}

void set3DViewingInformation(double xyz[6])
{
	// Tell class ModelView we initially want to see the whole scene:
	ModelView::setMCRegionOfInterest(xyz);

	// Two common computations to help determine a reasonable initial view:
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

	// COMMON HEURISTIC FOR ESTABLISHING THE MC ==> EC TRANSFORMATION:
	// Create the line of sight through the center of the scene:
	// 1) Make the center of attention be the center of the bounding box.
	// 2) Move the eye away along some direction - here (0,0,1) - so that the
	//    distance between the eye and the center is (2*max scene dimension).
	// 3) Set the "up" direction vector to orient the 3D view
	// NOTE: ModelView::getMatrices - used during display callbacks -
	//       implicitly assumes the line of sight passes through what we want
	//       to have in the center of the display window.

	// 1:
	cryph::AffPoint center(xmid, ymid, zmid);

	// 2:
	double distEyeCenter = 2.0 * maxDelta;
	cryph::AffPoint eye(xmid, ymid, zmid + distEyeCenter);

	// 3:
	cryph::AffVector up = cryph::AffVector::yu;

	// Notify the ModelView of our MC->EC viewing requests:
	ModelView::setEyeCenterUp(eye, center, up);

	// COMMON HEURISTIC FOR ESTABLISHING THE PROJECTION TRANSFORMATION:
	// Place the projection plane roughly at the front of scene and
	// set eye coordinate zmin/zmax clipping planes relative to it.
	// IMPORTANT NOTE: For perspective projections, the following must hold:
	// 1) zpp < 0
	// 2) zmin < zmax < 0
	// For non-perspective projections, it is only necessary that zmin < zmax.
	double zpp = -(distEyeCenter - 0.5*maxDelta);
	ModelView::setProjectionPlaneZ(zpp);
	double zmin = zpp - 1.5*maxDelta;
	double zmax = zpp + 0.5*maxDelta;
	ModelView::setECZminZmax(zmin, zmax);
	ModelView::setProjection(PERSPECTIVE);
}

int main(int argc, char* argv[])
{
	int rcFlags = MVC_USE_DEPTH_BIT;
	GLFWController c(argv[0], rcFlags);
	c.reportVersions(std::cout);
	ShaderIF* sIF = new ShaderIF("shaders/basic.vsh", "shaders/phong.fsh");

	createRoom(c, sIF);

	glClearColor(1.0, 1.0, 1.0, 1.0);

	double xyz[6];
	c.getOverallMCBoundingBox(xyz);
	set3DViewingInformation(xyz);

	c.run();

	delete sIF;

	return 0;
}
