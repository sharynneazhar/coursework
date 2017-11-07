// Block.c++

#include "Block.h"

typedef float vec3[3];

// index lists for the three faces that can't be drawn with glDrawArrays
GLuint Block::indexList[3][4] = {
	{ 6, 7, 0, 1 }, // xmin face
	{ 6, 0, 4, 2 }, // ymin face
	{ 1, 7, 3, 5 }  // ymax face
};

Block::Block(ShaderIF* sIF, PhongMaterial&matl,
						 float cx, float cy, float cz,
						 float lx, float ly, float lz) :
	SceneElement(sIF, matl)
{
	xmin = cx; xmax = cx + lx;
	ymin = cy; ymax = cy + ly;
	zmin = cz; zmax = cz + lz;
	defineBlock();
}

Block::~Block()
{
	glDeleteBuffers(3,ebo);
	glDeleteBuffers(1, vbo);
	glDeleteVertexArrays(1, vao);
}

void Block::defineBlock()
{
	vec3 vtx[] = { // The 8 unique vertices (Note the order)
		{xmin ,ymin, zmax}, {xmin, ymax, zmax},
		{xmax, ymin, zmax}, {xmax, ymax, zmax},
		{xmax, ymin, zmin}, {xmax, ymax, zmin},
		{xmin, ymin, zmin}, {xmin, ymax, zmin}
	};

	glGenVertexArrays(1, vao);
	glBindVertexArray(vao[0]);

	glGenBuffers(1, vbo);
	glBindBuffer(GL_ARRAY_BUFFER, vbo[0]);
	glBufferData(GL_ARRAY_BUFFER, 8 * sizeof(vec3), vtx, GL_STATIC_DRAW);
	glVertexAttribPointer(shaderIF->pvaLoc("mcPosition"), 3, GL_FLOAT, GL_FALSE, 0, 0);
	glEnableVertexAttribArray(shaderIF->pvaLoc("mcPosition"));

	glGenBuffers(3, ebo);
	for (int i = 0 ; i < 3 ; i++)
	{
		glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, ebo[i]);
		glBufferData(GL_ELEMENT_ARRAY_BUFFER, 4 * sizeof(GLuint), indexList[i], GL_STATIC_DRAW);
	}

	glDisableVertexAttribArray(shaderIF->pvaLoc("mcNormal"));
}

// xyzLimits: {mcXmin, mcXmax, mcYmin, mcYmax, mcZmin, mcZmax}
void Block::getMCBoundingBox(double* xyzLimits) const
{
	xyzLimits[0] = xmin;
	xyzLimits[1] = xmax;
	xyzLimits[2] = ymin;
	xyzLimits[3] = ymax;
	xyzLimits[4] = zmin;
	xyzLimits[5] = zmax;
}

bool Block::handleCommand(unsigned char anASCIIChar, double ldsX, double ldsY)
{
	// Block does not look for events; just hand off to inherited handleCommand.
	return this->ModelView::handleCommand(anASCIIChar, ldsX, ldsY);
}

void Block::render()
{
	// 1. Save current and establish new current shader program
	GLint pgm;
	glGetIntegerv(GL_CURRENT_PROGRAM, &pgm);
	glUseProgram(shaderIF->getShaderPgmID());

	// 2. Establish the SceneElement
	SceneElement::establishLightingEnvironment();
	SceneElement::establishView();
	SceneElement::establishMaterial();

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
