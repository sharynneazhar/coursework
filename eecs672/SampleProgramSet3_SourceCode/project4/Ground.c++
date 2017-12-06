// Ground.c++

#include "Ground.h"

typedef float vec3[3];
typedef float vec2[2];

PhongMaterial groundPhong(0.0, 0.1, 0.0, 0.3, 0.3, 0.3, 12, 1);

// index lists for the three faces that can't be drawn with glDrawArrays
GLuint Ground::indexList[3][4] = {
	{ 6, 7, 0, 1 }, // xmin face
	{ 6, 0, 4, 2 }, // ymin face
	{ 1, 7, 3, 5 }  // ymax face
};

Ground::Ground(ShaderIF* sIF, float width, float depth) :
	SceneElement(sIF, groundPhong)
{
	xyz[0] = 0.0; xyz[1] = 0.0 + width;
	xyz[2] = 0.0; xyz[3] = 0.0 + 0.15;
	xyz[4] = 0.0; xyz[5] = 0.0 + depth;
	defineInitialGeometry();
	setTextureImage("images/ground-texture.jpg");
}

Ground::~Ground()
{
	glDeleteBuffers(3, ebo);
	glDeleteBuffers(2, vbo);
	glDeleteVertexArrays(1, vao);
}

void Ground::defineInitialGeometry()
{
	vec3 mcPosition[] = { // The 8 unique vertices (Note the order)
		{xyz[0], xyz[2], xyz[5]}, {xyz[0], xyz[3], xyz[5]},
		{xyz[1], xyz[2], xyz[5]}, {xyz[1], xyz[3], xyz[5]},
		{xyz[1], xyz[2], xyz[4]}, {xyz[1], xyz[3], xyz[4]},
		{xyz[0], xyz[2], xyz[4]}, {xyz[0], xyz[3], xyz[4]}
	};

	// SO: https://stackoverflow.com/questions/3870437/repeating-textures-in-opengl
	vec2 texCoords[] = {
		{0, 0}, {0, 4}, {4, 0}, {4, 4},
		{4, 4}, {4, 0}, {0, 4}, {0, 0},
	};

	glGenVertexArrays(1, vao);
	glBindVertexArray(vao[0]);

	glGenBuffers(2, vbo);
	glBindBuffer(GL_ARRAY_BUFFER, vbo[0]);
	glBufferData(GL_ARRAY_BUFFER, 6 * sizeof(vec3), mcPosition, GL_STATIC_DRAW);
	glVertexAttribPointer(shaderIF->pvaLoc("mcPosition"), 3, GL_FLOAT, GL_FALSE, 0, 0);
	glEnableVertexAttribArray(shaderIF->pvaLoc("mcPosition"));

	glBindBuffer(GL_ARRAY_BUFFER, vbo[1]);
  glBufferData(GL_ARRAY_BUFFER, 16 * sizeof(vec2), texCoords, GL_STATIC_DRAW);
  glVertexAttribPointer(shaderIF->pvaLoc("texCoords"), 2, GL_FLOAT, GL_FALSE, 0, 0);
  glEnableVertexAttribArray(shaderIF->pvaLoc("texCoords"));

	glGenBuffers(3, ebo);
	for (int i = 0 ; i < 3 ; i++)
	{
		glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, ebo[i]);
		glBufferData(GL_ELEMENT_ARRAY_BUFFER, 8 * sizeof(GLuint), indexList[i], GL_STATIC_DRAW);
	}

	glDisableVertexAttribArray(shaderIF->pvaLoc("mcNormal"));
}

void Ground::getMCBoundingBox(double* xyzLimits) const
{
	for (int i = 0; i < 6; i++)
		xyzLimits[i] = xyz[i];
}

bool Ground::handleCommand(unsigned char anASCIIChar, double ldsX, double ldsY)
{
	// Ground does not look for events; just hand off to inherited handleCommand.
	return this->SceneElement::handleCommand(anASCIIChar, ldsX, ldsY);
}

void Ground::render()
{
	// 1. Save current and establish new current shader program
	GLint pgm;
	glGetIntegerv(GL_CURRENT_PROGRAM, &pgm);
	glUseProgram(shaderIF->getShaderPgmID());

	// 2. Establish the SceneElement
	establishTexture();
	establishLightingEnvironment();
	establishView();
	establishMaterial();

	// 4. Establish any other attributes and make one or more calls to
	//    glDrawArrays and/or glDrawElements
	//    If all or part of this model involves texture mapping, complete the
	//    implementation of SceneElement::establishTexture and call it from
	//    here as needed immediately before any glDrawArrays and/or glDrawElements
	//    calls to which texture is to be applied.
	glBindVertexArray(vao[0]);
	glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);

	// The three faces that can be drawn with glDrawArrays
	glVertexAttrib3f(shaderIF->pvaLoc("mcNormal"), 0.0, 0.0, 1.0);
	glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
	glVertexAttrib3f(shaderIF->pvaLoc("mcNormal"), 1.0, 0.0, 0.0);
	glDrawArrays(GL_TRIANGLE_STRIP, 2, 4);
	glVertexAttrib3f(shaderIF->pvaLoc("mcNormal"), 0.0, 0.0, -1.0);
	glDrawArrays(GL_TRIANGLE_STRIP, 4, 4);

	// The three faces that are drawn with glDrawElements
	glVertexAttrib3f(shaderIF->pvaLoc("mcNormal"), -1.0, 0.0, 0.0);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, ebo[0]);
	glDrawElements(GL_TRIANGLE_STRIP, 4, GL_UNSIGNED_INT, nullptr);
	glVertexAttrib3f(shaderIF->pvaLoc("mcNormal"), 0.0, -1.0, 0.0);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, ebo[1]);
	glDrawElements(GL_TRIANGLE_STRIP, 4, GL_UNSIGNED_INT, nullptr);
	glVertexAttrib3f(shaderIF->pvaLoc("mcNormal"), 0.0, 1.0, 0.0);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, ebo[2]);
	glDrawElements(GL_TRIANGLE_STRIP, 4, GL_UNSIGNED_INT, nullptr);

	// 5. Reestablish previous shader program
	glUseProgram(pgm);
}
