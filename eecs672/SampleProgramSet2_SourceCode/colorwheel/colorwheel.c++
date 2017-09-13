// colorwheel.c++: An OpenGL program that draws a colored circle

#include "GLFWController.h"
#include "ModelView.h"

int main(int argc, char* argv[])
{
	GLFWController c(argv[0]);

	cryph::AffPoint center( 0.0, 0.0, 0.0 );
	cryph::AffVector u = cryph::AffVector::xu; // (1, 0, 0)
	cryph::AffVector v = cryph::AffVector::yu; // (0, 1, 0)
	double radius = 1.0;

	ShaderIF* sIF = new ShaderIF("shaders/colorwheel.vsh", "shaders/colorwheel.fsh");

	ModelView* mv = new ModelView(sIF, center, u, v, radius);

	c.addModel( mv );

	glClearColor(1.0, 1.0, 1.0, 1.0);

	double xyz[6];
	c.getOverallMCBoundingBox(xyz);
	ModelView::setMCRegionOfInterest(xyz);

	c.run();

	delete sIF;

	return 0;
}
