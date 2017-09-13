// twoTriangles.c++ (V1)

#include "GLFWController.h"
#include "ModelView.h"

int main(int argc, char* argv[])
{
	// define vertex coordinates for the two triangles
	vec2 vertexPositions[][3] =
	{
		// triangle 1:
		{ { -6.0, 137.0 }, { 2.0, 137.0 }, { -2.0, 145.0 } },
		// triangle 2:
		{ { -6.0, 135.0 }, { 2.0, 135.0 }, { -2.0, 127.0 } }
	};

	GLFWController c("twoTriangles: V1");
	c.reportVersions(std::cout);
	ShaderIF* sIF = new ShaderIF("shaders/twoTriangles_V1.vsh", "shaders/twoTriangles_V1.fsh");
	c.addModel( new ModelView(sIF, vertexPositions[0]) );
	c.addModel( new ModelView(sIF, vertexPositions[1]) );

	// initialize 2D viewing information:
	// Get the overall scene bounding box in Model Coordinates:
	double xyz[6]; // xyz limits, even though this is 2D
	c.getOverallMCBoundingBox(xyz);
	// Tell class ModelView we initially want to see the whole scene:
	ModelView::setMCRegionOfInterest(xyz);

	glClearColor(1.0, 1.0, 1.0, 1.0);

	c.run();

	delete sIF;

	return 0;
}
