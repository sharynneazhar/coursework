// bowtie.c++: A variation of the circle example that makes a colorful bowtie

#include "GLFWController.h"
#include "ModelView.h"

int main(int argc, char* argv[])
{
	GLFWController c(argv[0]);
	c.reportVersions(std::cout);

	cryph::AffPoint center( 0.0, 0.0, 0.0 );
	// We can twist our bow tie around. Try different values
	// for u, making sure that at least one of the x and/or
	// y components != 0:
	cryph::AffVector u(1, 0.8, 0);
	cryph::AffVector v = cryph::AffVector::zu.cross(u);
	double radius = 1.0;
	// "a" is the "pinch factor" for the bowtie: 0 < a < 1.
	double a = 0.075;

	std::string font;
	if (argc > 1)
		font = std::string(argv[1]);
	else
		font = "../fontutil/fonts/ComicSansRegular36.fnt";

	ShaderIF* sIF = new ShaderIF("shaders/bowtie.vsh", "shaders/bowtie.fsh");

	ModelView* mv = new ModelView(sIF, center, u, v, radius, a, font);
	c.addModel( mv );

	glClearColor(1.0, 1.0, 1.0, 1.0);

	double xyz[6];
	c.getOverallMCBoundingBox(xyz);
	ModelView::setMCRegionOfInterest(xyz);

	c.run();

	delete sIF;

	return 0;
}
