// SceneElement.h - a base class that factors out many common data and
//                  method implementations to support a Phong local
//                  lighting model.

#ifndef SCENEELEMENT_H
#define SCENEELEMENT_H

#ifdef __APPLE_CC__
#include "GLFW/glfw3.h"
#else
#include <GL/gl.h>
#endif

#include "ModelView.h"
#include "PhongMaterial.h"
#include "ShaderIF.h"

static const int MAX_NUM_LIGHTS = 3; // MUST BE KEPT SAME AS IN the shader program

class SceneElement : public ModelView
{
public:
	SceneElement(ShaderIF* sIF, const PhongMaterial& matlIn);
	virtual ~SceneElement();

	void establishLightingEnvironment();
	void establishMaterial();
	void establishTexture();
	void establishView();

	bool handleCommand(unsigned char anASCIIChar, double ldsX, double ldsY);

	void setColorGenerationMode(int mode, int onFace=0);
	void setTextureImage(const std::string& imgFileName, int onFace=0);
	void setTextureSource(int source, int onFace=0);

protected:
	ShaderIF* shaderIF;
	PhongMaterial matl;

private:
	GLuint texID;
	int colorGenerationMode;
	int textureSource;

	// lighting environment
	static float lightPos[4*MAX_NUM_LIGHTS]; // (x,y,z,w) for each light
	static bool posInModelCoordinates[MAX_NUM_LIGHTS]; // pos is in MC or EC?
	static float lightStrength[3*MAX_NUM_LIGHTS]; // (r,g,b) for each light
	static float globalAmbient[3]; // (r,g,b) for ambient term, A
};

#endif
