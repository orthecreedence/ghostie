#version 330

uniform sampler2D tex;
uniform sampler2D depth;

out vec4 color;

smooth in vec2 texcoord;

void main()
{
	vec4 c	=	texture(tex, texcoord);
	// grabbing values out of the depth buffer causes program to fail.
	//vec2 z	=	texture(depth, texcoord);
	color	=	texture(tex, texcoord);
}

