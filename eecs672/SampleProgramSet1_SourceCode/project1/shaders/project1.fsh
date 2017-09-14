#version 410 core

uniform vec3 lineColor;

out vec4 fragmentColor;

void main()
{
	fragmentColor = vec4(lineColor, 1.0);
}
