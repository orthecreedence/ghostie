#version 330

layout(location = 0) in vec4 position;
layout(location = 1) in vec2 texmap;

uniform mat4 cameraToClipMatrix;

smooth out vec2 texcoord;

void main()
{
	gl_Position = position * cameraToClipMatrix;
	texcoord = texmap;
}
