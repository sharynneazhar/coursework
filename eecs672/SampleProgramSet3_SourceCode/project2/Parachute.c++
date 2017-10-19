// Parachute.c++

#include "Parachute.h"

const int N_POINTS_AROUND_SLICE = 20;

Parachute::Parachute(ShaderIF* sIF, cryph::AffPoint bottomIn, double radiusIn, double heightIn) : shaderIF(sIF), radius(radiusIn)
{
	kd[0] = 0.6;
	kd[1] = 0.8;
	kd[2] = 0.8;

	bottom = bottomIn;
	top.x = bottom.x;
	top.y = bottom.y;
	top.z = bottom.z + heightIn;
	axis = bottom - top;
	axis.normalize();

	defineParachute();
}

Parachute::~Parachute()
{
	glDeleteBuffers(2, vbo);
	glDeleteVertexArrays(1, vao);
}

void Parachute::defineParachute() {
	xyz[0] = bottom.x - radius;
	xyz[1] = bottom.x + radius;
	xyz[2] = bottom.y - radius;
	xyz[3] = bottom.y + radius;
	xyz[4] = bottom.z;
	xyz[5] = top.z;

	int totalPoints = N_POINTS_AROUND_SLICE + 2;

	vec3* coords = new vec3[totalPoints];
	vec3* normals = new vec3[totalPoints];

	double theta = 0.0;
	double dTheta = 2.0 * M_PI / N_POINTS_AROUND_SLICE;

	cryph::AffVector U(1, 0, 0); //x-axis
	cryph::AffVector V(0, 1, 0); //y-axis

	cryph::AffPoint first = bottom + radius * (cos(theta) * U + sin(theta) * V);
	cryph::AffVector first2 = first - bottom;
	first2.normalize();

	top.aCoords(coords, 0);
	first.aCoords(coords, totalPoints - 1);

	normals[0][0] = axis.dx;
	normals[0][1] = axis.dy;
	normals[0][2] = axis.dz;

	normals[totalPoints - 1][0] = first2.dx;
	normals[totalPoints - 1][1] = first2.dy;
	normals[totalPoints - 1][2] = first2.dz;

	for(int i = 1; i <= N_POINTS_AROUND_SLICE; i++)
	{
		cryph::AffPoint p = bottom + radius * (cos(theta) * U + sin(theta) * V);
		p.aCoords(coords, i);

		theta += dTheta;

		cryph::AffVector Vhat = top - p;
		cryph::AffVector VParallel = (axis.dot(Vhat)) * axis;
		cryph::AffVector vPerp = Vhat - VParallel;

		vPerp.normalize();
		normals[i][0] = vPerp.dx;
		normals[i][1] = vPerp.dy;
		normals[i][2] = vPerp.dz;
	}

	glGenVertexArrays(1, vao);
	glGenBuffers(2, vbo);

	glBindVertexArray(vao[0]);
	glBindBuffer(GL_ARRAY_BUFFER, vbo[0]);

	glBufferData(GL_ARRAY_BUFFER, totalPoints * sizeof(vec3), coords, GL_STATIC_DRAW);
	glVertexAttribPointer(shaderIF->pvaLoc("mcPosition"), 3, GL_FLOAT, GL_FALSE, 0, 0);
	glEnableVertexAttribArray(shaderIF->pvaLoc("mcPosition"));

	glBindBuffer(GL_ARRAY_BUFFER, vbo[1]);
	glBufferData(GL_ARRAY_BUFFER, totalPoints * sizeof(vec3), normals, GL_STATIC_DRAW);
	glVertexAttribPointer(shaderIF->pvaLoc("mcNormal"), 3, GL_FLOAT, GL_FALSE, 0, 0);
	glEnableVertexAttribArray(shaderIF->pvaLoc("mcNormal"));

	delete[] coords;
	delete[] normals;
}

// xyzLimits: {mcXmin, mcXmax, mcYmin, mcYmax, mcZmin, mcZmax}
void Parachute::getMCBoundingBox(double* xyzLimits) const
{
	xyzLimits[0] = xyz[0];
	xyzLimits[1] = xyz[1];
	xyzLimits[2] = xyz[2];
	xyzLimits[3] = xyz[3];
	xyzLimits[4] = xyz[4];
	xyzLimits[5] = xyz[5];
}

void Parachute::render()
{
	// 1. Save current and establish new current shader program
	GLint pgm;
	glGetIntegerv(GL_CURRENT_PROGRAM, &pgm);
	glUseProgram(shaderIF->getShaderPgmID());

	// 2. Establish "mc_ec" and "ec_lds" matrices
	cryph::Matrix4x4 mc_ec, ec_lds;
	getMatrices(mc_ec, ec_lds);

	float mat[16];
	glUniformMatrix4fv(shaderIF->ppuLoc("mc_ec"), 1, false, mc_ec.extractColMajor(mat));
	glUniformMatrix4fv(shaderIF->ppuLoc("ec_lds"), 1, false, ec_lds.extractColMajor(mat));

	// 3. Set GLSL's "kd" variable using this object's "kd" instance variable
	glUniform3fv(shaderIF->ppuLoc("kd"), 1, kd);

	// 4. Establish any other attributes and make one or more calls to
	//    glDrawArrays and/or glDrawElements
	glBindVertexArray(vao[0]);
	glDrawArrays(GL_TRIANGLE_FAN, 0, N_POINTS_AROUND_SLICE + 2);

	// 5. Reestablish previous shader program
	glUseProgram(pgm);
}
