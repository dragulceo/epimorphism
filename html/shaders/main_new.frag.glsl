#define VOID_W -0.000001

precision mediump float;

#include colorspace
#include math

uniform sampler2D fb;
uniform float time;
uniform float kernel_dim;
uniform float[] par;
uniform vec2[] zn;

void main() {
  vec4 seed, frame, res, color;
  vec2 z, zz, seed_z, frame_z;

  // into z coordinates
  z = vec2(2.0, 2.0) * gl_FragCoord.xy / vec2(kernel_dim, kernel_dim) - vec2(1.0, 1.0);
  zz = z;

  // seed
  @@seed

  // get frame
  @@t
  frame_z = (z + vec2(1.0, 1.0)) / vec2(2.0, 2.0);
  frame = texture2D(fb, frame_z);

  // blend
  res = vec4(seed.a * seed.rgb + (1.0 - seed.a) * frame.rgb, seed.a);

  // color
  @@color

  gl_FragColor = color;
}
