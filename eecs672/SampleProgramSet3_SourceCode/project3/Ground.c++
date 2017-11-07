// Ground.c++

#include "Ground.h"

Ground::Ground(ShaderIF* sIF, const PhongMaterial& matlIn,
  double xmin, double xmax, double ymin, double ymax) :
	SceneElement(sIF, matl), nx(0.0), ny(0.0), nz(1.0)
{
  xyz[0] = xmin; xyz[1] = xmax;
	xyz[2] = ymin; xyz[3] = ymax;
	xyz[4] = 0.0;  xyz[5] = 0.0;

	initGround();
}

Ground::~Ground()
{
  glDeleteBuffers(1, &vbo);
	glDeleteVertexArrays(1, &vao);
}

void Ground::initGround()
{
  double mcPosition[4][3] =
	{
		{ xyz[0], xyz[2], 0.0 }, { xyz[0], xyz[3], 0.0 },
		{ xyz[1], xyz[2], 0.0 }, { xyz[1], xyz[3], 0.0 }
	};

	glGenVertexArrays(1, &vao);
	glBindVertexArray(vao);

  glGenBuffers(1, &vbo);
  glBindBuffer(GL_ARRAY_BUFFER, vbo);

	glBufferData(GL_ARRAY_BUFFER, 4 * 3 * sizeof(float), mcPosition, GL_STATIC_DRAW);
	glVertexAttribPointer(shaderIF->pvaLoc("mcPosition"), 3, GL_FLOAT, GL_FALSE, 0, 0);
	glEnableVertexAttribArray(shaderIF->pvaLoc("mcPosition"));
  glDisableVertexAttribArray(shaderIF->pvaLoc("mcNormal"));
}

// xyzLimits: {mcXmin, mcXmax, mcYmin, mcYmax, mcZmin, mcZmax}
void Ground::getMCBoundingBox(double* xyzLimits) const
{
  for (int i = 0 ; i < 6; i++)
		xyzLimits[i] = xyz[i];
}

void Ground::render()
{
	// 1. Save current and establish new current shader program
	GLint pgm;
	glGetIntegerv(GL_CURRENT_PROGRAM, &pgm);
	glUseProgram(shaderIF->getShaderPgmID());

	// 2. Establish the SceneElement
	establishLightingEnvironment();
	establishView();
	establishMaterial();

	// 4. Establish any other attributes and make one or more calls to
	//    glDrawArrays and/or glDrawElements
	//    If all or part of this model involves texture mapping, complete the
	//    implementation of SceneElement::establishTexture and call it from
	//    here as needed immediately before any glDrawArrays and/or glDrawElements
	//    calls to which texture is to be applied.
	glBindVertexArray(vao);

  // set the normal to the floor
  glDisableVertexAttribArray(shaderIF->pvaLoc("mcNormal"));
  glVertexAttrib3f(shaderIF->pvaLoc("mcNormal"), nx, ny, nz);	glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);

	// 5. Reestablish previous shader program
	glUseProgram(pgm);
}
