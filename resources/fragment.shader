#version 300 es

precision mediump float;
in vec4 fnormal;
in vec4 v;
out vec4 fColor;

void main()
{
    vec4 light = vec4(5.0, 5.0, 1.0, 1.0);
    vec4 L = normalize(light - v);
    vec3 Idiff = vec3(1.0, 1.0, 1.0) * max(dot(fnormal,L), 0.1);
    Idiff = clamp(Idiff, 0.0, 1.0);
    vec4 I = vec4(Idiff, 1.0);
    fColor = I;
}
