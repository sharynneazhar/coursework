// SceneElement.c++

#include "SceneElement.h"
#include "ImageReader.h"

float SceneElement::lightPos[4 * MAX_NUM_LIGHTS] =
	{
		-10.0, -5.0, -10.0, 1.0,
		35.0, -5.0, -10.0, 1.0,
		0.0, 1.2, 0.7, 0.0
	};

// Are coordinates in "lightPos" stored in MC or EC?
bool SceneElement::posInModelCoordinates[MAX_NUM_LIGHTS] =
	{ true, true, false };
// The following is the buffer actually sent to GLSL. It will contain a copy of
// the (x,y,z,w) for light sources defined in EC; it will contain the coordinates
// after transformation to EC if the position was originally specified in MC.
float posToGLSL[4*MAX_NUM_LIGHTS];

float SceneElement::lightStrength[3 * MAX_NUM_LIGHTS] =
	{
		0.2, 0.2, 1.0,
		1.0, 0.2, 1.0,
		1.0, 1.0, 1.0
	};

float SceneElement::globalAmbient[] = { 0.2, 0.2, 0.2 };

SceneElement::SceneElement(ShaderIF* sIF, const PhongMaterial& matlIn) :
	shaderIF(sIF), matl(matlIn), texID(0), colorGenerationMode(-1),
	textureSource(-1)
{
}

SceneElement::~SceneElement()
{
}

void SceneElement::establishLightingEnvironment()
{
	// This should set:
	// "actualNumLights", "ecLightPosition", "lightStrength", "globalAmbient"

	// Copy "lightPos" to local array "lightPositionInEC".
  // While doing so, if any of the light sources are defined in MC,
  // transform them to EC. Then, send the EC geometric description
  // along with the non-geometric data:
	cryph::Matrix4x4 mc_ec, ec_lds;
	getMatrices(mc_ec, ec_lds);

	float lightPositionInEC[MAX_NUM_LIGHTS * 4];
	for (int i = 0; i < (MAX_NUM_LIGHTS * 4); i++)  {
		if (posInModelCoordinates[i] == true) {
			cryph::AffPoint p(lightPos[4 * i], lightPos[4 * i + 1], lightPos[4 * i + 2]);
			p = mc_ec * p;
			lightPositionInEC[4 * i] = p.x;
			lightPositionInEC[4 * i + 1] = p.y;
			lightPositionInEC[4 * i + 2] = p.z;
			lightPositionInEC[4 * i + 3] = lightPos[4 * i + 3];
		} else {
			lightPositionInEC[4 * i] = lightPos[4 * i];
			lightPositionInEC[4 * i + 1] = lightPos[4 * i + 1];
			lightPositionInEC[4 * i + 2] = lightPos[4 * i + 2];
			lightPositionInEC[4 * i + 3] = lightPos[4 * i + 3];
		}
	}

	glUniform1i(shaderIF->ppuLoc("actualNumLights"), MAX_NUM_LIGHTS);
  glUniform4fv(shaderIF->ppuLoc("lightPosition"), MAX_NUM_LIGHTS, lightPos);
  glUniform3fv(shaderIF->ppuLoc("lightStrength"), MAX_NUM_LIGHTS, lightStrength);
  glUniform3fv(shaderIF->ppuLoc("globalAmbient"), 1, globalAmbient);
}

void SceneElement::establishMaterial()
{
	glUniform3fv(shaderIF->ppuLoc("ka"), 1, matl.ka);
	glUniform3fv(shaderIF->ppuLoc("kd"), 1, matl.kd);
  glUniform3fv(shaderIF->ppuLoc("ks"), 1, matl.ks);
	glUniform1f(shaderIF->ppuLoc("alpha"), matl.alpha);
  glUniform1f(shaderIF->ppuLoc("shininess"), matl.shininess);
}

void SceneElement::establishTexture()
{
	// Unless you are texture-mapping onto faces of BasicShape instances,
	// this method should be called from your render method, and it should
	// set texture-related parameters like:
	// "colorGenerationMode", "textureSource", "textureMap"
	// It should also do the appropriate call to glBindTexture.
	// (If you are texture-mapping onto faces of BasicShape instances,
	// you use the "prepareForFace" callback which may or may not be
	// implemented by calling this method.)
}

void SceneElement::establishView()
{
	// Line of sight, dynamic view controls, 3D-2D projection, & mapping to LDS:
	float m[16];
	cryph::Matrix4x4 mc_ec, ec_lds;
	ModelView::getMatrices(mc_ec, ec_lds);
	glUniformMatrix4fv(shaderIF->ppuLoc("mc_ec"), 1, false, mc_ec.extractColMajor(m));
	glUniformMatrix4fv(shaderIF->ppuLoc("ec_lds"), 1, false, ec_lds.extractColMajor(m));

	if (ModelView::projType == PERSPECTIVE) {
		glUniform1i(shaderIF->ppuLoc("projection"), 1);
	} else if(ModelView::projType == ORTHOGONAL) {
		glUniform1i(shaderIF->ppuLoc("projection"), 2);
	} else if (ModelView::projType == OBLIQUE) {
		glUniform1i(shaderIF->ppuLoc("projection"), 3);
	}
}

bool SceneElement::handleCommand(unsigned char anASCIIChar, double ldsX, double ldsY)
{
	if (anASCIIChar == 'O')
		ModelView::setProjection(ORTHOGONAL);
	else if (anASCIIChar == 'P')
		ModelView::setProjection(PERSPECTIVE);
	else if (anASCIIChar == 'Q')
		ModelView::setProjection(OBLIQUE);
	else
		return ModelView::handleCommand(anASCIIChar, ldsX, ldsY);
	Controller::getCurrentController()->redraw();
	return true;
}

void SceneElement::setColorGenerationMode(int mode, int onFace)
{
	colorGenerationMode = mode;
}

void SceneElement::setTextureImage(const std::string& imgFileName, int onFace)
{
	ImageReader* ir = ImageReader::create(imgFileName.c_str());
	if (ir == nullptr)
	{
		std::cerr << "Could not open '" << imgFileName << "' for texture map.\n";
		return;
	}
	glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
	glGenTextures(1, &texID);
	glBindTexture(GL_TEXTURE_2D, texID);
	float white[] = { 1.0, 1.0, 1.0, 1.0 };
	glTexParameterfv(GL_TEXTURE_2D, GL_TEXTURE_BORDER_COLOR, white);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE); //GL_CLAMP_TO_BORDER);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE); //GL_CLAMP_TO_BORDER);
	GLint level = 0;
	int pw = ir->getWidth(), ph = ir->getHeight();
	GLint iFormat = ir->getInternalFormat();
	GLenum format = ir->getFormat();
	GLenum type = ir->getType();
	const GLint border = 0; // must be zero (only present for backwards compatibility)
	const void* pixelData = ir->getTexture();
	glTexImage2D(GL_TEXTURE_2D, level, iFormat, pw, ph, border, format, type, pixelData);
	delete ir;
}

void SceneElement::setTextureSource(int source, int onFace)
{
	textureSource = source;
}
