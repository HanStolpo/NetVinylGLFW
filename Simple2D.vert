#version 330

layout(location = 0) in vec2 vPosition2D;

uniform mat4 mWorldViewProj2D;

void main()
{

    gl_Position =  vec4(vPosition2D.xy, 0, 1) * mWorldViewProj2D;
}
