#version 330

//smooth in vec4 theColor;
out vec4 outputColor;

void main()
{
	float mixc = (abs(gl_FragCoord.z) - .5) * .5;

	outputColor = mix(
		vec4(1.0f, 1.0f, 1.0f, 1.0f),
		vec4(0.0f, 0.0f, 0.0f, 1.0f),
		1.0 - mixc
	);
}

