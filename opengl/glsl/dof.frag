#version 330

smooth in vec2 texcoord;

// FIX THESE
uniform int renderTexWidth;
uniform int renderTexHeight;
uniform int mouse_x;
uniform int mouse_y;

uniform sampler2D renderTex;
uniform sampler2D depthTex;

out vec4 colorOut;

#define PI 3.141592654

int rings			=	6;
int samples			=	10;
float focalPoint	=	0;
float amt			=	0.008;
float threshold		=	.001;

void main()
{
	colorOut	=	vec4(texture2D(renderTex, texcoord).rgb, 1.0);
}

/*
void main()
{
	// grabbing values out of the depth buffer causes program to fail.
	float z			=	texture2D(depthTex, texcoord).x;
	vec3 col		=	texture2D(renderTex, texcoord).rgb;
	vec3 orig_color	=	col;

	focalPoint	=	texture2D(depthTex, vec2(0.5, 0)).x;
	//focalPoint	=	0.9999;

	float w	=	1 / 600;
	float h	=	1 / 600;

	int ringsamples;
	vec3 sample;
	float avg		=	1.0;
	float z_diff	=	clamp(abs(focalPoint - z) / 2, 0.0, 1.0);
	float rad_const	=	.008;
	if(z_diff >= threshold)
	{
		rings	=	clamp(int(rings * z_diff * 10), 0, rings);
		rings	=	rings;
		for(int i = 0; i < rings; i++)
		{
			ringsamples		=	samples + i;
			float step		=	(PI * 2.0) / float(ringsamples);
			float weight	=	0;

			for(int r = 0; r < ringsamples; r++)
			{
				float rad	=	i + 1;
				float rx	=	cos(float(r) * step) * rad * rad_const;
				float ry	=	sin(float(r) * step) * rad * rad_const;
				vec2 pos	=	abs(texcoord + vec2(rx, ry));
				float zpos	=	texture2D(depthTex, pos).x;
				sample		=	texture2D(renderTex, pos).rgb;
				weight		=	0; //abs(zpos - z);
				col	+=	mix(sample, orig_color, weight) * 1;
				avg	+=	1;
			}
		}
	}
	colorOut = vec4(col / avg, 1.0);
}
*/
