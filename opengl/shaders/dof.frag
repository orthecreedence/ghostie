uniform sampler2D renderTex;
uniform sampler2D depthTex;

out vec4 color;

smooth in vec2 texcoord;

void main()
{
	vec4 c	=	texture(renderTex, texcoord);
	// grabbing values out of the depth buffer causes program to fail.
	float z	=	texture(depthTex, texcoord).x;
	color	=	texture(renderTex, texcoord) + (z * 0.000001);
}
