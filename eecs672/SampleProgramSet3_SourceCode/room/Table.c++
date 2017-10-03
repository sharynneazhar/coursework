// Table.c++

#include "Table.h"

Table::Table(ShaderIF* sIF, const PhongMaterial& matlIn,
		cryph::AffPoint corner, cryph::AffVector u,
		double legHeight, double legRadius,
		double tableWidth, double tableDepth, double tableThickness)
		 : SceneElement(sIF, matlIn)
{
	defineInitialGeometry(corner, u, legHeight, legRadius,
		tableWidth, tableDepth, tableThickness);
	xyz[0] = 1.0; xyz[1] = 0.0;
	for (int i=0 ; i<5 ; i++)
		if (pieces[i] == nullptr)
			piecesR[i] = nullptr;
		else
		{
			piecesR[i] = new BasicShapeRenderer(sIF, pieces[i]);
			// accumulate bounding box
			if (xyz[0] > xyz[1]) // not yet initialized
				pieces[i]->getMCBoundingBox(xyz);
			else
			{
				double thisxyz[6];
				pieces[i]->getMCBoundingBox(thisxyz);
				for (int j=0 ; j<3 ; j++)
				{
					if (thisxyz[2*j] < xyz[2*j])
						xyz[2*j] = thisxyz[2*j];
					if (thisxyz[2*j+1] > xyz[2*j+1])
						xyz[2*j+1] = thisxyz[2*j+1];
				}
			}
		}
}

Table::~Table()
{
	for (int i=0 ; i<5 ; i++)
	{
		if (pieces[i] != nullptr)
			delete pieces[i];
		if (piecesR[i] != nullptr)
			delete piecesR[i];
	}
}

void Table::defineInitialGeometry(cryph::AffPoint corner, cryph::AffVector u,
		double legHeight, double legRadius,
		double tableWidth, double tableDepth, double tableThickness)
{
	// we assume the table is parallel to xy-plane, hence:
	cryph::AffVector uu(u[0], u[1], 0.0), ww(0,0,1);
	uu.normalize();
	cryph::AffVector vv = ww.cross(uu);
	pieces[0] = BasicShape::makeBlock(corner+legHeight*ww,
		uu, tableWidth, vv, tableDepth, ww, tableThickness);
	cryph::AffPoint bottom = corner + 2.0 * legRadius * (uu + vv);
	cryph::AffPoint top = bottom + legHeight * ww;
	pieces[1] = BasicShape::makeBoundedCylinder(bottom, top,
		legRadius, 20, 2, BasicShape::CAP_AT_NEITHER);
	bottom += (tableWidth - 4.0 * legRadius) * uu;
	top = bottom + legHeight * ww;
	pieces[2] = BasicShape::makeBoundedCylinder(bottom, top,
		legRadius, 20, 2, BasicShape::CAP_AT_NEITHER);
	bottom += (tableDepth - 4.0 * legRadius) * vv;
	top = bottom + legHeight * ww;
	pieces[3] = BasicShape::makeBoundedCylinder(bottom, top,
		legRadius, 20, 2, BasicShape::CAP_AT_NEITHER);
	bottom -= (tableWidth - 4.0 * legRadius) * uu;
	top = bottom + legHeight * ww;
	pieces[4] = BasicShape::makeBoundedCylinder(bottom, top,
		legRadius, 20, 2, BasicShape::CAP_AT_NEITHER);
}

void Table::getMCBoundingBox(double* xyzLimits) const
{
	for (int i=0 ; i<6 ; i++)
		xyzLimits[i] = xyz[i];
}

void Table::prepareForFace(void* caller, int faceIndex)
{
	if (caller == nullptr)
		// No caller passed to "drawShape"; cannot proceed
		return;
	Table* thisTable = reinterpret_cast<Table*>(caller);
	thisTable->setColorGenerationMode(0); // just light model
	if (thisTable->currentlyDrawingPiece == 0) // The block table top
	{
		if (faceIndex == 5) // wmax face; the top table surface
		{
			thisTable->piecesR[0]->setTexCoordsForBlock(faceIndex);
			thisTable->setTextureSource(0); // texture map
			thisTable->setColorGenerationMode(20); // light model * texture
		}
	}
	thisTable->establishTexture();
}

void Table::render()
{
	GLint pgm;
	glGetIntegerv(GL_CURRENT_PROGRAM, &pgm);
	glUseProgram(shaderIF->getShaderPgmID());

	establishLightingEnvironment();
	establishMaterial();
	establishView();

	for (currentlyDrawingPiece=0 ; currentlyDrawingPiece<5 ; currentlyDrawingPiece++)
		if (piecesR[currentlyDrawingPiece] != nullptr)
			piecesR[currentlyDrawingPiece]->drawShape(prepareForFace, this);

	// Ensure no other part of the scene is given texture unless it asks for it
	setColorGenerationMode(0); // just light model

	glUseProgram(pgm);
}
