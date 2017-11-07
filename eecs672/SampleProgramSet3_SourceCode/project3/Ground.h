// Ground.h

#ifndef GROUND_H
#define GROUND_H

#include "SceneElement.h"
#include "ShaderIF.h"

class Ground : public SceneElement
{
public:
	Ground(ShaderIF* sIF, const PhongMaterial& matlIn,
         double xmin, double xmax, double ymin, double ymax);

	virtual ~Ground();

	void getMCBoundingBox(double* xyzLimits) const; // {xmin, xmax, ymin, ymax, zmin, zmax}
	void render();

private:
  GLuint vao;
	GLuint vbo;
  double xyz[6];
	double nx, ny, nz;

	void initGround();
};

#endif
