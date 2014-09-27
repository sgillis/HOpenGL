#version 300 es

precision highp float;
in vec4 position;
in vec4 normal;

uniform mat4 camera;
uniform mat4 projection;
uniform mat4 model;

out vec4 fnormal;
out vec4 v;

void main()
{
    v = model * position;
    gl_Position = projection * camera * v;
    fnormal = normal;
}
