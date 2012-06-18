#version 330

smooth in vec2 texcoord;

uniform sampler2D renderTex;
uniform sampler2D depthTex;
uniform float renderTexWidth;
uniform float renderTexHeight;
uniform float amt;

out vec4 colorOut;

#define PI 3.141592654

int rings = 5;
int samples = 5;
float rad = .005;
float focalDist = .0;

void main()
{
	// grabbing values out of the depth buffer causes program to fail.
	float z	=	texture2D(depthTex, texcoord).x;
	vec3 col = texture2D(renderTex, texcoord).rgb;

	float w = 1.0 / renderTexWidth;
	float h = 1.0 / renderTexHeight;
	int ringsamples;
	float blur	=	abs(focalDist - (z * .04));
	for(int i = 1; i <= rings; i++)
	{
		ringsamples = i * samples;

		for(int r = 0; r < ringsamples; r++)
		{
			float step = PI * 2.0 / float(ringsamples);
			float rx = cos(float(r) * step) * float(i) * blur;
			float ry = sin(float(r) * step) * float(i) * blur;
			col += amt * clamp(z+.5, 0.0, 1.0) * texture2D(renderTex, abs(texcoord + vec2(rx, ry))).rbg;
		}
	}
	colorOut = vec4(col, 1.0);
}
