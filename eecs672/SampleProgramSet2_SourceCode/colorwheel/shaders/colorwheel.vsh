#version 410 core

in vec3 mcPosition;
in vec3 vertexColor;
uniform vec4 scaleTrans;

out vec3 color;

void main()
{
	color = vertexColor;
	float ldsX = scaleTrans[0]*mcPosition.x + scaleTrans[1];
	float ldsY = scaleTrans[2]*mcPosition.y + scaleTrans[3];
	gl_Position = vec4(ldsX, ldsY, mcPosition.z, 1);
}
