// TEMPLATE_Subclass.c++

#include "TEMPLATE_Subclass.h"

TEMPLATE_Subclass::TEMPLATE_Subclass(ShaderIF* sIF, PhongMaterial& matl) : SceneElement(sIF, matl)
{
}

TEMPLATE_Subclass::~TEMPLATE_Subclass()
{
}

// xyzLimits: {mcXmin, mcXmax, mcYmin, mcYmax, mcZmin, mcZmax}
void TEMPLATE_Subclass::getMCBoundingBox(double* xyzLimits) const
{
	xyzLimits[0] = -1000.0; // xmin  Give real values!
	xyzLimits[1] = 1000.0;  // xmax         |
	xyzLimits[2] = -1234.5; // ymin         |
	xyzLimits[3] = -1011.2; // ymax         |
	xyzLimits[4] = -3000.0; // zmin         |
	xyzLimits[5] = -2000.0; // zmax        \_/
}

void TEMPLATE_Subclass::render()
{
	// 1. Save current and establish new current shader program
	// ...

	// 2. Establish "mc_ec" and "ec_lds" matrices
	//    call SceneElement::establishView(); to do this

	// 3. Set GLSL's "kd" variable using this object's "kd" instance variable
	//    complete the implementation of SceneElement::establishMaterial and then
	//    call it from here.

	// 4. Establish any other attributes and make one or more calls to
	//    glDrawArrays and/or glDrawElements
	//    If all or part of this model involves texture mapping, complete the
	//    implementation of SceneElement::establishTexture and call it from
	//    here as needed immediately before any glDrawArrays and/or glDrawElements
	//    calls to which texture is to be applied.

	// 5. Reestablish previous shader program
	// ...
}
