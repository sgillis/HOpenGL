#version 300 es

precision mediump float;
in vec4 fNormal;
in vec4 fPosition;
out vec4 fColor;

void main()
{
    vec3 s = vec3(5.0, 10.0, 10.0);
    vec3 l = normalize(s - vec3(fPosition));
    vec3 n = normalize(vec3(fNormal));
    vec3 Idiff = vec3(1.0, 1.0, 1.0) * max(dot(n, l), 0.1);
    Idiff = clamp(Idiff, 0.0, 1.0);
    fColor = vec4(Idiff, 1.0);
}
