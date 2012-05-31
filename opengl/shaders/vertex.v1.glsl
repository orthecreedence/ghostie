#version 330

layout(location = 0) in vec4 position;

uniform vec3 offset;
uniform mat4 perspectiveMatrix;

void main()
{
	vec4 cameraPos = position + vec4(offset.x, offset.y, offset.z, 0);

	gl_Position = perspectiveMatrix * cameraPos;
}
