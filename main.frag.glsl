precision mediump float;

uniform sampler2D u_image;

uniform float time;
uniform float kernel_dim;

#define VOID_W -0.000001


float sinh(float x){
  return (exp(x) - exp(-1.0 * x)) / 2.0;
}

float cosh(float x){
  return (exp(x) + exp(-1.0 * x)) / 2.0;
}


void main() {
  vec2 z = vec2(2.0, 2.0) * gl_FragCoord.xy / vec2(kernel_dim, kernel_dim) - vec2(1.0, 1.0);

  vec2 z_in = z;
  float zxt = z.x;
  float zyt = z.y;

  z_in.x += time / 10.0; // zn[1] moves linearly

  vec4 res = get_frame(z_in);

  // seed_t
  z.x = zxt * cos(time / 15.0) + zyt * sin(time / 15.0);
  z.y = -1.0 * zxt * sin(time / 15.0) + zyt * cos(time / 15.0);
  z = z * e^(i * time / 15) // zn[0] moves on a circle of radius 1

  float s, c;
  z = zn[0] * sinhz(z) // zn circle of radius 1.1
  z = torus_reduce(z);

  z = (z + vec2(1.0, 1.0)) / vec2(2.0, 2.0);

  vec4 frame = texture2D(u_image, z);

  // blend
  res = vec4(res.a * res.rgb + (1.0 - res.a) * frame.rgb, res.a);

  // color
  vec3 color = rgb2hsv(res.rgb);
  color.r += 0.17;

  gl_FragColor = vec4(hsv2rgb(color), 1.0);
}
