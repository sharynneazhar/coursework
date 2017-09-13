// threeTriangles.c++: An OpenGL program that draws three colored triangles

#include "GLFWController.h"
#include "ModelView.h"

void makeFrame(Controller& c, ShaderIF* sIF, float halfWidth)
{
	float frameWidth = 0.125;
	float fs = (1.0 + frameWidth) * halfWidth; // "frame start"
	float fe = (1.0 + 2.0*frameWidth) * halfWidth; // "frame end"
	vec2 frameCoords[] =
	{
		{ -fe, -fe }, { -fs, -fs }, {  fe, -fe }, {  fs, -fs },
		{  fe,  fe }, {  fs,  fs }, { -fe,  fe }, { -fs,  fs },
		{ -fe, -fe }, { -fs, -fs }
	};

	float g = 0.5;
	vec3 frameColors[] =
	{
		{ 0.0, 0.0, 0.0 },{ g, g, g },{ 0.0, 0.0, 0.0 },{ g, g, g },
		{ 0.0, 0.0, 0.0 },{ g, g, g },{ 0.0, 0.0, 0.0 },{ g, g, g },
		{ 0.0, 0.0, 0.0 },{ g, g, g }
	};

	float outer = 1.0, inner = 0.0;
	float frameFraction[] =
	{
		outer, inner, outer, inner, outer,
		inner, outer, inner, outer, inner
	};

	c.addModel( new ModelView(sIF, frameCoords, frameColors, frameFraction, 10) );
}

void makeTree(Controller& c, ShaderIF* sIF, float xs, float ys, float outer[], float inner[])
{
	// define per-vertex colors
	// (Whether these will actually be used depends on the current "colorMode".)
	vec3 vertexColors[20] =
	{
		// bottom four: black trunk
		{ 0.0, 0.0, 0.0 }, { 0.0, 0.0, 0.0 },
		{ 0.0, 0.0, 0.0 }, { 0.0, 0.0, 0.0 }
	};
	// alternate outer and inner for the rest
	for (int i=4 ; i<20 ; i+=4)
	{
		for (int j=0 ; j<3 ; j++)
		{
			vertexColors[i  ][j] = outer[j];
			vertexColors[i+1][j] = outer[j];
			vertexColors[i+2][j] = inner[j];
			vertexColors[i+3][j] = inner[j];
		}
	};

	// define vertex coordinates
	vec2 vertexPositions[] =
	{
		{-2, 0}, {2, 0}, {-2, 4}, {2, 4}, {-10, 6}, {10, 6}, {-4, 9}, {4, 9}, {-7, 11}, {7, 11},
		{-2, 13}, {2, 13}, {-4, 14}, {4, 14}, {-1, 16}, {1, 16}, {-2, 17}, {2, 17},
		{ -0.2, 19}, {0.2, 19}
	};

	float heightFractions[20];

	for (int i=0 ; i<20 ; i++)
	{
		heightFractions[i] = vertexPositions[i][1] / 19.0;
		vertexPositions[i][0] += xs;
		vertexPositions[i][1] += ys;
	}

	c.addModel( new ModelView(sIF, vertexPositions, vertexColors, heightFractions, 20) );
}

int main(int argc, char* argv[])
{
	GLFWController c(argv[0]);
	c.reportVersions(std::cout);

	ShaderIF* sIF = new ShaderIF("shaders/fancytree.vsh", "shaders/fancytree.fsh");

	float outer0[] = { 0.0, 1.0, 0.0 };
	float inner0[] = { 1.0, 0.0, 0.0 };
	makeTree(c, sIF, 7, -3, outer0, inner0);
	float outer1[] = { 0.0, 1.0, 0.0 };
	float inner1[] = { 0.0, 0.2, 0.0 };
	makeTree(c, sIF, -9.5, -8, outer1, inner1);
	float outer2[] = { 0.0, 0.0, 1.0 };
	float inner2[] = { 0.0, 0.0, 0.3 };
	makeTree(c, sIF, 0, -10, outer2, inner2);
	makeFrame(c, sIF, 18);

	glClearColor(1.0, 1.0, 1.0, 1.0);

	// initialize 2D viewing information:
	// Get the overall scene bounding box in Model Coordinates:
	double xyz[6]; // xyz limits, even though this is 2D
	c.getOverallMCBoundingBox(xyz);
	// Tell class ModelView we initially want to see the whole scene:
	ModelView::setMCRegionOfInterest(xyz);

	c.run();

	delete sIF;

	return 0;
}
