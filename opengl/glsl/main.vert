#version 330

layout(location = 0) in vec4 position;

uniform mat4 modelToCameraMatrix;
uniform mat4 cameraToClipMatrix;

void main()
{
	vec4 cameraPos	=	position * modelToCameraMatrix;
	gl_Position		=	cameraPos * cameraToClipMatrix;
}
