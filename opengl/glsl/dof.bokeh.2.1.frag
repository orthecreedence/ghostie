#version 330

uniform sampler2D renderTex;
uniform sampler2D depthTex;
uniform float renderTexWidth;
uniform float renderTexHeight;
smooth in vec2 texcoord;

#define PI  3.14159265

float width = renderTexWidth; //texture width
float height = renderTexHeight; //texture height

vec2 texel = vec2(1.0/width,1.0/height);

//uniform float focalDepth;  //external focal point value, but you may use autofocus option below
float focalDepth = 3.5;

//------------------------------------------
//user variables

int samples = 1; //samples on the first ring
int rings = 5; //ring count

bool autofocus = false; //use autofocus in shader? disable if you use external focalDepth value
vec2 focus = vec2(0.5,0.5); // autofocus point on screen (0.0,0.0 - left lower corner, 1.0,1.0 - upper right)
float range = 4.0; //focal range
float maxblur = 1.25; //clamp value of max blur

float threshold = 0.821; //highlight threshold;
float gain = 0.0; //highlight gain;

float bias = 10.4; //bokeh edge bias
float fringe = 1.0; //bokeh chromatic aberration/fringing

bool noise = false; //use noise instead of pattern for sample dithering
float namount = 0.0001; //dither amount

bool depthblur = false; //blur the depth buffer?
float dbsize = 1.0; //depthblursize

//------------------------------------------


float bdepth(vec2 coords) //blurring depth
{
	float d = 0.0;
	float kernel[9];
	vec2 offset[9];
	
	vec2 wh = vec2(texel.x, texel.y) * dbsize;
	
	offset[0] = vec2(-wh.x,-wh.y);
	offset[1] = vec2( 0.0, -wh.y);
	offset[2] = vec2( wh.x -wh.y);
	
	offset[3] = vec2(-wh.x,  0.0);
	offset[4] = vec2( 0.0,   0.0);
	offset[5] = vec2( wh.x,  0.0);
	
	offset[6] = vec2(-wh.x, wh.y);
	offset[7] = vec2( 0.0,  wh.y);
	offset[8] = vec2( wh.x, wh.y);
	
	kernel[0] = 1.0/16.0;   kernel[1] = 2.0/16.0;   kernel[2] = 1.0/16.0;
	kernel[3] = 2.0/16.0;   kernel[4] = 4.0/16.0;   kernel[5] = 2.0/16.0;
	kernel[6] = 1.0/16.0;   kernel[7] = 2.0/16.0;   kernel[8] = 1.0/16.0;
	
	
	for( int i=0; i<9; i++ )
	{
		float tmp = texture2D(depthTex, coords + offset[i]).r;
		d += tmp * kernel[i];
	}
	
	return d;
}


vec3 color(vec2 coords,float blur) //processing the sample
{
	vec3 col = vec3(0.0);
	
	col.r = texture2D(renderTex,coords + vec2(0.866,-0.5)*texel*fringe*blur).r;
	col.g = texture2D(renderTex,coords + vec2(-0.866,-0.5)*texel*fringe*blur).g;
	col.b = texture2D(renderTex,coords + vec2(0.866,-0.5)*texel*fringe*blur).b;
	
	vec3 lumcoeff = vec3(0.299,0.587,0.114);
	float lum = dot(col.rgb, lumcoeff);
	float thresh = max((lum-threshold)*gain, 0.0);
	return col+mix(vec3(0.0),col,thresh*blur);
}

vec2 rand(in vec2 coord) //generating noise/pattern texture for dithering
{
	float noiseX = ((fract(1.0-coord.s*(width/2.0))*0.25)+(fract(coord.t*(height/2.0))*0.75))*2.0-1.0;
	float noiseY = ((fract(1.0-coord.s*(width/2.0))*0.75)+(fract(coord.t*(height/2.0))*0.25))*2.0-1.0;
	
	if (noise)
	{
		noiseX = clamp(fract(sin(dot(coord ,vec2(12.9898,78.233))) * 43758.5453),0.0,1.0)*2.0-1.0;
		noiseY = clamp(fract(sin(dot(coord ,vec2(12.9898,78.233)*2.0)) * 43758.5453),0.0,1.0)*2.0-1.0;
	}
	return vec2(noiseX,noiseY);
}

void main() 
{
	
	float depth = texture2D(depthTex,texcoord.xy).x;
	float blur = 0.0;
	
	if (depthblur)
	{
		depth = bdepth(texcoord.xy);
	}
	
	blur = clamp((abs(depth - focalDepth)/range)*100.0,-maxblur,maxblur);
	
	if (autofocus)
	{
		float fDepth = texture2D(depthTex,focus).x;
		blur = clamp((abs(depth - fDepth)/range)*100.0,-maxblur,maxblur);
	}
	
	vec2 noise = rand(texcoord.xy)*namount*blur;
	
	float w = (1.0/width)*blur+noise.x;
	float h = (1.0/height)*blur+noise.y;
	
	vec3 col = texture2D(renderTex, texcoord.xy).rgb;
	float s = 1.0;
	
	int ringsamples;
	
	for (int i = 1; i <= rings; i += 1)
	{   
		ringsamples = i * samples;
		 
		for (int j = 0 ; j < ringsamples ; j += 1)   
		{
			float step = PI*2.0 / float(ringsamples);
			float pw = (cos(float(j)*step)*float(i));
			float ph = (sin(float(j)*step)*float(i));
			float p = 1.0;
			col += color(texcoord.xy + vec2(pw*w,ph*h),blur)*mix(1.0,(float(i))/(float(rings)),bias)*p;  
			s += 1.0*mix(1.0,(float(i))/(float(rings)),bias)*p;   
		}
	}
	
	
	col /= s;   
	
	gl_FragColor.rgb = col;
	gl_FragColor.a = 1.0;
}

