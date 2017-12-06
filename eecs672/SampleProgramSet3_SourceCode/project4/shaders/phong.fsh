#version 410 core

// phong.fsh - a fragment shader that implements a Phong Lighting model.

in PVA {
	vec3 ecPosition;
	vec3 ecUnitNormal;
	vec2 texCoords;
} pvaIn;

out vec4 fragmentColor;

// Num of light sources
const int MAX_NUM_LIGHTS = 3;

// Phong material properties
uniform vec3 kd = // "kd" - diffuse reflectivity; basic object color
	vec3(0.8, 0.0, 0.0); // default: darkish red
uniform vec3 ka = // "ka" - ambient reflectivity; basic object color
	vec3(0.8, 0.0, 0.0); // default: darkish red
uniform vec3 ks = // "ks" - specular reflectivity; basic object color
	vec3(0.8, 0.0, 0.0); // default: darkish red

uniform int actualNumLights;
uniform vec4 lightPosition[MAX_NUM_LIGHTS];
uniform vec3 lightStrength[MAX_NUM_LIGHTS];
uniform vec3 globalAmbient;
uniform float alpha;
uniform float shininess;

uniform int textureFlag;
uniform sampler2D textureMap;

uniform mat4 ec_lds = // (W-V map) * (projection matrix)
	mat4(1.0, 0.0, 0.0, 0.0, // initialize to (almost) identity matrix
	     0.0, 1.0, 0.0, 0.0, // ==> ORTHOGONAL projection -AND- EC = LDS
	     0.0, 0.0, -1.0, 0.0,
	     0.0, 0.0, 0.0, 1.0);

vec4 evaluateLightingModel() {
	// NOTES:
	// 1. We assume for now a single directional light source defined in EC (liHat).
	// 2. We assume it will be "full strength" (see lightStrength).
	//
	// In project 3, both #1 and #2 will be generalized by introducing uniform
	// arrays ("vec4 p_ecLightSourcePos" and "vec3 ecLightSourceStrength") and
	// using them INSTEAD OF the liHat and lightStrength you see here.
	vec3 ec_Q = pvaIn.ecPosition;
	vec3 ec_nHat = pvaIn.ecUnitNormal;

	vec3 globalAmbient = ka * globalAmbient;
	vec3 diffuseVec = vec3(0.0, 0.0, 0.0);
	vec3 specularVec = vec3(0.0, 0.0, 0.0);

	vec3 vHat;

	// Create a unit vector towards the viewer (method depends on type of projection!)
	if (ec_lds[3][3] != 0.0f) { // perspective
		vHat = normalize(-ec_Q);
	} else if (ec_lds[1][0] != 0.0f) { // orthogonal
		vHat = vec3(0.0, 0.0, 1.0);
	} else { // oblique
		vHat = normalize(vec3((-ec_lds[2][0])/ec_lds[0][0], (-ec_lds[2][1])/ec_lds[1][1], 1.0));
	}

	// If we are viewing this point "from behind", we need to negate the incoming
  // normal vector since our lighting model expressions implicitly assume the normal
  // vector points toward the same side of the triangle that the eye is on.
	if (dot(ec_nHat, vHat) < 0) {
		ec_nHat = -ec_nHat;
	}

	for (int i = 0; i < actualNumLights; i++) {
		// if light is behind this object, skip this light source
    // else:
    //     1. compute and accumulate diffuse contribution
    //     2. if viewer on appropriate side of the primary reflection vector,
    //        compute and accumulate specular contribution.
		vec3 liHat = vec3(0.0, 0.0, 0.0);

		if (lightPosition[i].w == 0.0) { // directional light source
			liHat = normalize(lightPosition[i].xyz);
		} else { // positional light source
			liHat = normalize(lightPosition[i].xyz - ec_Q);
		}

		if (dot(liHat, ec_nHat) > 0) {
			vec3 riHat = normalize(reflect(-liHat, ec_nHat));
			float riDotV = dot(riHat, vHat);
			float dist = 0.5 * distance(lightPosition[i].xyz, ec_Q);
			float atten = (1 / dist) * 8;

			if (lightPosition[i].w == 0.0) {
				diffuseVec += kd * lightStrength[i] * dot(liHat, ec_nHat);
			} else {
				diffuseVec += atten * kd * lightStrength[i] * dot(liHat, ec_nHat);
			}

			if (riDotV > 0) {
				if (lightPosition[i].w == 0.0) {
					specularVec += ks * lightStrength[i] * pow(riDotV, shininess);
				} else {
					specularVec += atten * ks * lightStrength[i] * ks * pow(riDotV, shininess);
				}
			}
		}
	}

	vec3 lightTotal = globalAmbient + diffuseVec + specularVec;

	for (int i = 0; i < 3; i++) {
		if (lightTotal[i] >= 1.0) {
			lightTotal[i] = 1.0;
		}
	}

	return vec4(lightTotal, alpha);
}

vec4 composeColor(vec4 lmColor, vec4 tColor) {
	vec4 texColor = lmColor + tColor;
	return (textureFlag == 1) ? texColor : lmColor;
}

void main () {
	vec4 lightModelColor = evaluateLightingModel();
	vec4 textureColor = (textureFlag == 1) ? texture(textureMap, pvaIn.texCoords) : vec4(0.0);
	fragmentColor = composeColor(lightModelColor, textureColor);
}
