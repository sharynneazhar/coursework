// TEMPLATE_Subclass.h

#ifndef TEMPLATE_SUBCLASS_H
#define TEMPLATE_SUBCLASS_H

#include "ModelView.h"
#include "ShaderIF.h"

class TEMPLATE_Subclass : public ModelView
{
public:
	// As before: you will likely want to add parameters to the constructor
	TEMPLATE_Subclass(ShaderIF* sIF);
	virtual ~TEMPLATE_Subclass();

	// xyzLimits: {mcXmin, mcXmax, mcYmin, mcYmax, mcZmin, mcZmax}
	void getMCBoundingBox(double* xyzLimitsF) const;
	void render();
private:
	ShaderIF* shaderIF;
	float kd[3];
};

#endif
