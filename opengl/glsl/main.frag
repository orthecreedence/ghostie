#version 330

uniform vec4 colorIn;
uniform float fogAmt = 0.0;
uniform vec4 fogColor = vec4(0, 0, 0, 1);

layout(location = 0) out vec4 color;

void main()
{
	float fog_end	=	160;
	float fog_start	=	60; //72;
	float fog_dist	=	fog_end - fog_start;
	float fog_amt;
	float fog_coord;
	
	fog_coord	=	abs(gl_FragCoord.z / gl_FragCoord.w) - fog_start;
	fog_coord	=	clamp(fog_coord, 0.0, fog_dist);

	fog_amt		=	(fog_dist - fog_coord) / fog_dist;
	fog_amt		=	clamp(fog_amt, 0.0, 1.0);

	// TODO: use "discard" to not write to depth buffer if fragment has
	// certain alpha value:
	//   http://www.opengl.org/wiki/Transparency_Sorting

	color	=	mix(
		colorIn,
		fogColor,
		(1 - fog_amt) * fogAmt
	);
	gl_FragDepth	=	gl_FragCoord.z;
}

