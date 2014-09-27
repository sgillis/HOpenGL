#version 300 es

precision highp float;
in vec4 position;

uniform mat4 camera;
uniform mat4 projection;
uniform mat4 model;

void main()
{
    gl_Position = projection * camera * model * position;
}
