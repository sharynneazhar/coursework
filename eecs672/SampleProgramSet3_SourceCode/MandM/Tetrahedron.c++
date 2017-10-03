// Tetrahedron.c++

#include <iostream>
#include <math.h>

#include "Tetrahedron.h"

Tetrahedron::Tetrahedron(ShaderIF* sIF,
		const cryph::AffPoint& p0, const cryph::AffPoint& p1,
		const cryph::AffPoint& p2, const cryph::AffPoint& p3,
		float R, float G, float B) : shaderIF(sIF)
{
	kd[0] = R; kd[1] = G; kd[2] = B;
	// put vertices in array to simplify generation of geometry:
	cryph::AffPoint verts[] = { p0, p1, p2, p3 };
	defineTetrahedron(verts);
	// set min/max coordinates for MC bounding box:
	minMax[0] = minMax[1] = p0.x;
	minMax[2] = minMax[3] = p0.y;
	minMax[4] = minMax[5] = p0.z;
	updateXYZBounds(p1);
	updateXYZBounds(p2);
	updateXYZBounds(p3);
}

Tetrahedron::~Tetrahedron()
{
	glDeleteBuffers(1, vbo);
	glDeleteVertexArrays(1, vao);
}

void Tetrahedron::defineTetrahedron(const cryph::AffPoint verts[])
{
	typedef float vec3[3];

	// We need SIX vertices for GL_TRIANGLE_STRIP; the last two
	// are copies of the first two:
	vec3 vtx[6];
	for (int i=0 ; i<6 ; i++)
		// "i%4" because last two are copies of first two
		verts[i%4].aCoords(vtx, i);

	// There will be FOUR faces, so compute those FOUR normal vectors:
	for (int i=0 ; i<4 ; i++)
	{
		int viP1 = (i + 1) % 4;
		int viP2 = (i + 2) % 4;
		cryph::AffVector v01 = verts[viP1] - verts[i];
		cryph::AffVector v02 = verts[viP2] - verts[i];
		normal[i] = v01.cross(v02);
	}

	// ******************************************************************
	// EXERCISE: Do the usual VAO/VBO magic here so that:
	//           1) coordinates are sent in VBO
	//           2) Normals will NOT be sent in VBO; rather they will
	//              be set face-by-face (i.e., on a per-primitive basis)
	//              in Tetrahedron::renderTetrahedron
	// ******************************************************************
	std::cout << "Tetrahedron::defineTetrahedron: create/fill VBOs. You will see errors until you do so.\n";
}

void Tetrahedron::getMCBoundingBox(double* xyzLimits) const
{
	for (int i=0 ; i<6 ; i++)
		xyzLimits[i] = minMax[i];
}

bool Tetrahedron::handleCommand(unsigned char anASCIIChar, double ldsX, double ldsY)
{
	// Tetrahedron does not look for events. Just hand off back to inherited handleCommand.
	return this->ModelView::handleCommand(anASCIIChar, ldsX, ldsY);
}

void Tetrahedron::render()
{
	GLint pgm;
	glGetIntegerv(GL_CURRENT_PROGRAM, &pgm);
	glUseProgram(shaderIF->getShaderPgmID());

	cryph::Matrix4x4 mc_ec, ec_lds;
	getMatrices(mc_ec, ec_lds);
	float mat[16];
	glUniformMatrix4fv(shaderIF->ppuLoc("mc_ec"), 1, false, mc_ec.extractColMajor(mat));
	glUniformMatrix4fv(shaderIF->ppuLoc("ec_lds"), 1, false, ec_lds.extractColMajor(mat));

	glBindVertexArray(vao[0]);
	glUniform3fv(shaderIF->ppuLoc("kd"), 1, kd);

	// draw the four faces:
	for (int fi=0 ; fi<4 ; fi++)
	{
		glVertexAttrib3f(shaderIF->pvaLoc("mcNormal"), normal[fi].dx, normal[fi].dy, normal[fi].dz);
		glDrawArrays(GL_TRIANGLE_STRIP, fi, 3);
	}

	glUseProgram(pgm);
}

void Tetrahedron::updateXYZBounds(const cryph::AffPoint& p)
{
	if (p.x < minMax[0])
		minMax[0] = p.x;
	else if (p.x > minMax[1])
		minMax[1] = p.x;

	if (p.y < minMax[2])
		minMax[2] = p.y;
	else if (p.y > minMax[3])
		minMax[3] = p.y;

	if (p.z < minMax[4])
		 minMax[4] = p.z;
	else if (p.z > minMax[5])
		minMax[5] = p.z;
}
