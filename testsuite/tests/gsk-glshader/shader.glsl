//  Resource fixture for Gsk.GLShader constructor and metadata tests.
uniform bool u_enabled;
uniform float u_gain;
uniform int u_count;
uniform uint u_flags;
uniform vec2 u_offset;
uniform vec3 u_color;
uniform vec4 u_tint;
uniform sampler2D u_texture2;

void mainImage(out vec4 fragColor, in vec2 fragCoord,
               in vec2 resolution, in vec2 uv)
{
  fragColor = u_tint + vec4(u_color, 1.0) * u_gain;
}
