#version 410 core

in vec2 mcPosition; // vertex position; "mc" stands for "model coordinates"
uniform vec4 scaleTrans; // for mapping coordinates into Logical Device Space

void main()
{
//	      ldsX =      sx      *  mcX        +      tx
	float ldsX = scaleTrans[0]*mcPosition.x + scaleTrans[1];

//	      ldsY =      sy      *  mcY        +      ty
	float ldsY = scaleTrans[2]*mcPosition.y + scaleTrans[3];

	gl_Position = vec4(ldsX, ldsY, 0, 1);
}

