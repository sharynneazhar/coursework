#version 410 core

// phong.fsh - a fragment shader that implements a Phong Lighting model.

in PVA
{
	vec3 ecPosition;
	vec3 ecUnitNormal;
} pvaIn;

out vec4 fragmentColor;

// Phong material properties (just kd for now; you will add the rest later):
uniform vec3 kd = // "kd" - diffuse reflectivity; basic object color
	vec3(0.8, 0.0, 0.0); // default: darkish red

vec4 evaluateLightingModel()
{
	// THIS IS JUST A PLACEHOLDER FOR A LIGHTING MODEL.
	// It only currently implements simple Lambert shading.

	// NOTES:
	// 1. We assume for now a single directional light source defined in EC (liHat).
	// 2. We assume it will be "full strength" (see liStrength).
	//
	// In project 3, both #1 and #2 will be generalized by introducing uniform
	// arrays ("vec4 p_ecLightSourcePos" and "vec3 ecLightSourceStrength") and
	// using them INSTEAD OF the liHat and liStrength you see here.
	//
	// 3. The use of "abs" here is a temporary hack. As we study the Phong
	//    lighting model more carefully, you will REMOVE "abs" since it will
	//    no longer be appropriate.

	vec3 liHat = vec3(0.0, 0.0, 1.0);
	vec3 liStrength = vec3(1.0, 1.0, 1.0);
	float factor = abs(dot(liHat, pvaIn.ecUnitNormal));

	return vec4(factor * kd * liStrength, 1.0);
}

void main ()
{
	fragmentColor = evaluateLightingModel();
}
