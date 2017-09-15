// project1.c++

#include <iostream>
#include <fstream>

#include "GLFWController.h"
#include "ModelView.h"

int main(int argc, char* argv[])
{
	if (argc < 2) {
	 std::cout << "\n>>> No data file provided.";
	 std::cout << "\nUsage information: ./project1 <file>";
	 return 0;
	}

	std::ifstream file(argv[1]);
	if (!file.good()) {
		std::cout << "\nCould not open file " << argv[1] << ".\n";
		return 0;
	}

	GLFWController c(argv[0]);
	c.reportVersions(std::cout);

	ShaderIF* sIF = new ShaderIF("shaders/project1.vsh", "shaders/project1.fsh");

	float a0, a1, a2, a3;
	float b0, b1, b2, b3;
	float tmin, tmax;
	int nPoints;

	while(file >> a0) {
		file >> a1 >> a2 >> a3;
		file >> b0 >> b1 >> b2 >> b3;
		file >> tmin >> tmax;
		file >> nPoints;

		// (x(t), y(t)) = (a0 + a1t + a2t^2 + a3t^3, b0 + b1t + b2t^2 + b3t^3)
		vec2 curves[nPoints];
		float t = tmin; // t varies across parametric range: tmin ≤ t ≤ tmax
		float dt = (tmax - tmin) / (nPoints - 1);

		// Each curve is evaluated at the specified number of points
		// for that curve: tmin, tmin+dt, tmin+2*dt, …, tmax,
		// where dt=(tmax-tmin)/(nPoints-1).
		for (int i = 0; i < nPoints; i++) {
			curves[i][0] = a0 + (a1 * t) + (a2 * t * t) + (a3 * t * t * t);
			curves[i][1] = b0 + (b1 * t) + (b2 * t * t) + (b3 * t * t * t);
			t += dt;
		}

		c.addModel(new ModelView(sIF, curves, nPoints));
	}

	file.close();

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
