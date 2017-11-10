// Tree.c++

#include "Tree.h"

PhongMaterial treePhong(0.32, 0.2, 0.039, 1.0, 1.0);
PhongMaterial forestGreen(0.419608, 0.556863, 0.137255, 0.5, 0.5);
PhongMaterial forestYellow(0.219608, 0.556863, 0.137255, 0.5, 0.5);

Tree::Tree(ShaderIF* sIF, cryph::AffPoint point, double height) : SceneElement(sIF, treePhong)
{
	cryph::AffVector u(0.0, 1.0, 0.0);
	cryph::AffVector uu(u[0], u[1], 0.0), ww(0, 1, 0); uu.normalize();
	cryph::AffVector vv = ww.cross(uu);

	int nPointsAroundSide = 200;
	int nPointsAlongAxis = 20;
	double radius = 0.35;

	cryph::AffPoint bottom = point;
	cryph::AffPoint top = bottom + (height * ww);
	tree = BasicShape::makeBoundedCylinder(bottom, top, radius,
						                          	 nPointsAroundSide, nPointsAlongAxis,
																				 BasicShape::CAP_AT_BOTH);

	bool isEven = (int) point.x % 2 == 0;
	if (isEven)
		treeTop = new TreeTop(sIF, forestGreen, top, 0.9);
	else
		treeTop = new TreeTop(sIF, forestYellow, top, 0.9);

	xyz[0] = 1.0; xyz[1] = 0.0;

	if (tree == nullptr) {
		treeR = nullptr;
	} else {
		treeR = new BasicShapeRenderer(sIF, tree);
		if (xyz[0] > xyz[1]) { // not yet initialized
			tree->getMCBoundingBox(xyz);
		} else {
			double thisxyz[6];
			tree->getMCBoundingBox(thisxyz);
			for (int j = 0; j < 3; j++) {
				if (thisxyz[2 * j] < xyz[2 * j])
					xyz[2 * j] = thisxyz[2 * j];
				if (thisxyz[2 * j + 1] > xyz[2 * j + 1])
					xyz[2 * j + 1] = thisxyz[2 * j + 1];
			}
		}
	}
}

Tree::~Tree()
{
	if (tree != nullptr)
		delete tree;
	if (treeR != nullptr)
		delete treeR;
}

// xyzLimits: {mcXmin, mcXmax, mcYmin, mcYmax, mcZmin, mcZmax}
void Tree::getMCBoundingBox(double* xyzLimits) const
{
	for (int i = 0 ; i < 6; i++)
		xyzLimits[i] = xyz[i];
}

void Tree::render()
{
	// 1. Save current and establish new current shader program
	GLint pgm;
	glGetIntegerv(GL_CURRENT_PROGRAM, &pgm);
	glUseProgram(shaderIF->getShaderPgmID());

	// 2. Establish the SceneElement
	establishLightingEnvironment();
	establishView();
	establishMaterial();

	if (treeR != nullptr) {
		treeR->drawShape();
		treeTop->render();
	}


	// 5. Reestablish previous shader program
	glUseProgram(pgm);
}
