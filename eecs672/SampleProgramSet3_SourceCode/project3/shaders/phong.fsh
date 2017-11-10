#version 410 core

// phong.fsh - a fragment shader that implements a Phong Lighting model.

in PVA {
	vec3 ecPosition;
	vec3 ecUnitNormal;
	vec3 ecObliqueVec;
} pvaIn;

out vec4 fragmentColor;

// Light source
const int MAX_NUM_LIGHTS = 3;

// Phong material properties
uniform int projection;
uniform vec4 lightPosition[MAX_NUM_LIGHTS];
uniform vec3 lightStrength[MAX_NUM_LIGHTS];
uniform int actualNumLights;
uniform vec3 globalAmbient;
uniform vec3 ka, kd, ks;
uniform float alpha;
uniform float shininess;

vec4 evaluateLightingModel(in vec3 ec_Q, in vec3 ec_nHat) {
	// NOTES:
	// 1. We assume for now a single directional light source defined in EC (liHat).
	// 2. We assume it will be "full strength" (see lightStrength).
	//
	// In project 3, both #1 and #2 will be generalized by introducing uniform
	// arrays ("vec4 p_ecLightSourcePos" and "vec3 ecLightSourceStrength") and
	// using them INSTEAD OF the liHat and lightStrength you see here.

	vec3 globalAmbient = ka * globalAmbient;
	vec3 diffuseVec = vec3(0.0, 0.0, 0.0);
	vec3 specularVec = vec3(0.0, 0.0, 0.0);
	vec3 vHat;

	// Create a unit vector towards the viewer (method depends on type of projection!)
	if (projection == 1) { // perspective
		vHat = normalize(-ec_Q);
	} else if (projection == 2) { // orthogonal
		vHat = pvaIn.ecObliqueVec;
	} else {
		vHat = vec3(0.0, 0.0, 1.0);
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
    vec4 currentLightPos = lightPosition[i];
		vec3 liHat = vec3(0.0, 0.0, 0.0);

		if (currentLightPos.w == 0.0) {
			liHat = normalize(currentLightPos.xyz);
		} else {
			liHat = normalize(currentLightPos.xyz - ec_Q);
		}

		if (dot(liHat, ec_nHat) > 0) {
			vec3 riHat = normalize(reflect(-liHat, ec_nHat));
			float riDotV = dot(riHat, vHat);
			float dist = 0.5 * distance(currentLightPos.xyz, ec_Q);
			float atten = (1 / dist) * 25;

			if (currentLightPos.w == 0.0) {
				diffuseVec += kd * lightStrength[i] * dot(liHat, ec_nHat);
			} else {
				diffuseVec += atten * kd * lightStrength[i] * dot(liHat, ec_nHat);
			}

			if (riDotV > 0) {
				if (currentLightPos.w == 0.0) {
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

void main () {
	fragmentColor = evaluateLightingModel(pvaIn.ecPosition, pvaIn.ecUnitNormal);
}
