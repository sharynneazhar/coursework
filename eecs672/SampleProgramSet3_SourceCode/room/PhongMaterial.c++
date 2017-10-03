// PhongMaterial.c++ - Utility struct with Phong model properties

#include "PhongMaterial.h"

PhongMaterial::PhongMaterial(float r, float g, float b, float af, float df, float sf,
		float shine, float a) :
		shininess(shine), alpha(a)
{
	float rgb[] = { r, g, b};
	copyColors(rgb, af, df, sf);
}

PhongMaterial::PhongMaterial(float kaR, float kaG, float kaB,
	float kdR, float kdG, float kdB,
	float ksR, float ksG, float ksB, float m, float Alpha) :
		shininess(m), alpha(Alpha)
{
	ka[0] = kaR; ka[1] = kaG; ka[2] = kaB;
	kd[0] = kdR; kd[1] = kdG; kd[2] = kdB;
	ks[0] = ksR; ks[1] = ksG; ks[2] = ksB;
}

void PhongMaterial::copyColors(const float* rgb, float af, float df, float sf)
{
	if (rgb == nullptr)
		return;
	for (int i=0 ; i<3 ; i++)
	{
		ka[i] = af * rgb[i];
		kd[i] = df * rgb[i];
		ks[i] = sf * rgb[i];
	}
}
