#version 410 core

// phong.fsh - a fragment shader that implements a Phong Lighting model.

in PVA {
	vec3 ecPosition;
	vec3 ecUnitNormal;
} pvaIn;

out vec4 fragmentColor;

// Light source
const int MAX_NUM_LIGHTS = 3;

// Phong material properties
uniform int numLights;
uniform int projType;
uniform vec4 liPosition[MAX_NUM_LIGHTS];
uniform vec3 liStrength[MAX_NUM_LIGHTS];
uniform vec3 globalAmbient;
uniform vec3 ka;
uniform vec3 kd;
uniform vec3 ks;
uniform float m;

vec4 evaluateLightingModel() {
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

	vec3 diffuse = vec3(0.0, 0.0, 0.0);
	vec3 specular = vec3(0.0, 0.0, 0.0);

	vec3 vHat;
	vec3 ec_nHat;

	if (projType == 0) { // perspective
		vHat = -(normalize(pvaIn.ecPosition.xyz));
	} else if (projType == 1 || projType == 2) { // oblique or orthogonal
		vHat = vec3(0.0, 0.0, 1.0);
	}

	if (dot(pvaIn.ecUnitNormal, vHat) < 0) {
		ec_nHat = -pvaIn.ecUnitNormal;
	}

	for (int i = 0; i < numLights; i++) {
		vec3 liHat = vec3(0.0, 0.0, 0.0);
		vec4 currLightPos = liPosition[i];

		if (currLightPos.w == 0.0) {
			liHat = normalize(currLightPos.xyz);
		} else {
			liHat = normalize(currLightPos.xyz - pvaIn.ecPosition.xyz);
		}

		if (dot(liHat, ec_nHat) > 0) {
			vec3 riHat = normalize(reflect(vHat, pvaIn.ecUnitNormal));
			float riDotV = dot(riHat, vHat);
			float atten = 1;

			if (liPosition[i][3] == 1) {
				float dist = distance(liPosition[i].xyz, pvaIn.ecPosition.xyz);
				atten = 1 / dist;
			}

			if (riDotV > 0) {
				specular += atten * liStrength[i] * (ks * pow(riDotV, m));
			}

			diffuse += atten * liStrength[i] * (kd * dot(pvaIn.ecUnitNormal, liHat));
		}
	}

	vec3 retVal = ka * globalAmbient + (diffuse + specular);

	float maxVal = 0.0;
	for (int i = 0; i < length(retVal); i++) {
		if (retVal[i] > maxVal) {
			maxVal = retVal[i];
		}
	}

	if (maxVal > 1.0) {
		retVal = retVal / maxVal;
	}

	return vec4(retVal, 1.0);
}

void main () {
	fragmentColor = evaluateLightingModel();
}
