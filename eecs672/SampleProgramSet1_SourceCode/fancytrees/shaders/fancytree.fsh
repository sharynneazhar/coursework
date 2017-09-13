#version 410 core

in PVA
{
	vec3 color;
	float fraction;
} pvaIn;

// colorMode:
//   0 ==> use interpolated vertex color (colorToFS)
//   1 ==> use interpolated height fraction
//   2 ==> use product of two
//   3 ==> hard-wired constant darkish magenta
uniform int colorMode;

out vec4 fragmentColor;

void main()
{
	if (colorMode == 0)
		fragmentColor = vec4(pvaIn.color,1.0);
	else if (colorMode == 1)
		fragmentColor = vec4(pvaIn.fraction, pvaIn.fraction, pvaIn.fraction, 1.0);
	else if (colorMode == 2)
		fragmentColor = vec4(pvaIn.fraction*pvaIn.color, 1.0);
	else // colorMode == 3
		fragmentColor = vec4(0.6, 0.0, 0.6, 1.0);
}
