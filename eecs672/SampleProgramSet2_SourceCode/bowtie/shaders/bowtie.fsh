#version 410 core

in PVA
{
	vec2 texCoords;
	vec3 color;
} pvaIn;

// uniforms for Font rendering
uniform int renderingFontString;
uniform sampler2D fontTextureMap;
uniform vec4 fontColor;
// END: uniforms for Font Rendering

out vec4 fragmentColor;

void main()
{
	if (renderingFontString == 1)
		fragmentColor = texture(fontTextureMap, pvaIn.texCoords) * fontColor;
	else
		fragmentColor = vec4(pvaIn.color, 1.0);
}
