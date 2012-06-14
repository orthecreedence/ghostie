#version 330

uniform sampler2D tex;
out vec4 color;

smooth in vec2 texcoord;

void main()
{
	color	=	texture(tex, texcoord);
}

