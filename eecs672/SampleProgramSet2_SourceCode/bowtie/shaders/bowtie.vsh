#version 410 core

layout (location = 0) in vec2 mcPosition;
in vec2 texCoords;
in vec3 vertexColor;

uniform vec4 scaleTrans;

out PVA
{
	vec2 texCoords;
	vec3 color;
} pvaOut;

void main()
{
	pvaOut.texCoords = texCoords;
	pvaOut.color = vertexColor;
	float ldsX = scaleTrans[0]*mcPosition.x + scaleTrans[1];
	float ldsY = scaleTrans[2]*mcPosition.y + scaleTrans[3];
	gl_Position = vec4(ldsX, ldsY, 0, 1);
}
