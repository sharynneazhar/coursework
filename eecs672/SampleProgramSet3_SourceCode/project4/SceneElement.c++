// SceneElement.c++

#include "SceneElement.h"
#include "ImageReader.h"

float SceneElement::lightPos[4 * MAX_NUM_LIGHTS] =
	{
	  -9.5, 1.15, -55.75, 1.0,    // blue light
		23.0, 1.15, -20.75, 1.0,  // purple light
		8.25, 55.5, -2.0, 0.0  // directional "moon" light
	};

// Are coordinates in "lightPos" stored in MC or EC?
bool SceneElement::posInModelCoordinates[MAX_NUM_LIGHTS] =
	{ true, true, true };

// The following is the buffer actually sent to GLSL. It will contain a copy of
// the (x,y,z,w) for light sources defined in EC; it will contain the coordinates
// after transformation to EC if the position was originally specified in MC.
float posToGLSL[4 * MAX_NUM_LIGHTS];

float SceneElement::lightStrength[3 * MAX_NUM_LIGHTS] =
{
	0.0, 0.0, 2.6, // blue light
	2.8, 0.0, 2.0, // purple light
	1.4, 1.4, 1.4  // greyish moon light
};

float SceneElement::globalAmbient[] = { 0.75, 0.75, 0.75 };

SceneElement::SceneElement(ShaderIF* sIF, const PhongMaterial& matlIn) :
	shaderIF(sIF), matl(matlIn), texID(0), colorGenerationMode(-1),
	textureSource(-1), wrapS(GL_REPEAT), wrapT(GL_REPEAT)
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
	int activeTexture = texID; // We will study this under "advanced texture mapping"
	glActiveTexture(GL_TEXTURE0 + activeTexture);

	// Set the textureMap uniform variable in the fragment shader so
	// that it will use the current "texture unit" we just specified above.
	glUniform1i(shaderIF->ppuLoc("textureMap"), activeTexture);

	// glBindTexture reestablishes the current texture along with its settings.
	glBindTexture(GL_TEXTURE_2D, texID);

	// If any texture parameters need to be set (or if ones set when the texture
	// was defined have changed), we set them here with their current values.
	// For example:
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, wrapS);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, wrapT);

	// Depending on how you write your fragment shader, you will probably need to
	// set one or more uniforms here to tell your fragment shader things like
	// (i) whether there is currently a texture map to use, (ii) how you want to
	// use the color from the texture map, (iii) etc.

	// So set them...
	glUniform1i(shaderIF->ppuLoc("textureFlag"), 1);
}

void SceneElement::establishView()
{
	// Line of sight, dynamic view controls, 3D-2D projection, & mapping to LDS:
	float m[16];
	cryph::Matrix4x4 mc_ec, ec_lds;
	ModelView::getMatrices(mc_ec, ec_lds);
	glUniformMatrix4fv(shaderIF->ppuLoc("mc_ec"), 1, false, mc_ec.extractColMajor(m));
	glUniformMatrix4fv(shaderIF->ppuLoc("ec_lds"), 1, false, ec_lds.extractColMajor(m));
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
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT); //GL_CLAMP_TO_BORDER);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT); //GL_CLAMP_TO_BORDER);
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
