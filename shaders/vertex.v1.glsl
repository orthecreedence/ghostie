#version 330

layout(location = 0) in vec3 position;
void main()
{
	gl_Position.xyz = position;
}
