precision mediump float;

uniform sampler2D u_image;

uniform float time;
uniform float kernel_dim;

#define VOID_W -0.000001

vec3 rgb2hsv(vec3 c)
{
    vec4 K = vec4(0.0, -1.0 / 3.0, 2.0 / 3.0, -1.0);
    vec4 p = c.g < c.b ? vec4(c.bg, K.wz) : vec4(c.gb, K.xy);
    vec4 q = c.r < p.x ? vec4(p.xyw, c.r) : vec4(c.r, p.yzx);

    float d = q.x - min(q.w, q.y);
    float e = 1.0e-10;
    return vec3(abs(q.z + (q.w - q.y) / (6.0 * d + e)), d / (q.x + e), q.x);
}


vec3 hsv2rgb(vec3 c)
{
    vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
    vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
    return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
}

vec2 torus_reduce(vec2 z){
  z = z + vec2(1.0, 1.0);

  z = mod(z, 4.0);
  if(z.x >= 2.0)
    z.x = 4.0 - z.x;
  if(z.y >= 2.0)
    z.y = 4.0 - z.y;

  return z - vec2(1.0, 1.0);
}


vec4 get_frame(vec2 z){
  float seed_w = 0.4;
  z = torus_reduce(z);

  float w = VOID_W;
  float wx = (z.x + seed_w) / (2.0 * seed_w);
  float wy = (z.y + seed_w) / (2.0 * seed_w);

  if(z.x >= z.y && z.x >= -1.0 * z.y && z.x > (1.0 - seed_w)){
    w = (z.x - (1.0 - seed_w)) / seed_w;
    wy = z.y / (0.5 * seed_w) - 0.5;
  }else if(z.y >= z.x && z.y >= -1.0 * z.x && z.y > (1.0 - seed_w)){
    w = (z.y - (1.0 - seed_w)) / seed_w;
    wy = z.x / (0.5 * seed_w) - 0.5;
  }else if(z.y >= z.x && z.y <= -1.0 * z.x && z.x < -1.0 * (1.0 - seed_w)){
    w = (-1.0 * (1.0 - seed_w) - z.x) / seed_w;
    wy = z.y / (0.5 * seed_w) - 0.5;
  }else if(z.x >= z.y && z.x <= -1.0 * z.y && z.y < -1.0 * (1.0 - seed_w)){
    w = (-1.0 * (1.0 - seed_w) - z.y) / seed_w;
    wy = z.x / (0.5 * seed_w) - 0.5;
  }
  wx = w;

  vec4 frame_w = vec4(w, (abs(w-VOID_W) < 0.00001 ? 0.0 : w * 0.7), wx, wy);
  return vec4(frame_w.x, 0, 0, frame_w.y);
}

float sinh(float x){
  return (exp(x) - exp(-1.0 * x)) / 2.0;
}

float cosh(float x){
  return (exp(x) + exp(-1.0 * x)) / 2.0;
}


void main() {
  vec2 z = vec2(2.0, 2.0) * gl_FragCoord.xy / vec2(kernel_dim, kernel_dim) - vec2(1.0, 1.0);

  //z.x += 0.5;
  //z.x += cos(t / 20000.0);
  //z.y += sin(t / 20000.0);
  //z.y += 1.0;
  //float tmp = z.x;
  //z.x = z.y;
  //  z.y = -1.0 * tmp;
  vec2 tz = z;
  float zxt = z.x;
  float zyt = z.y;

  z.x = zxt * cos(time / 15.0) + zyt * sin(time / 15.0);
  z.y = -1.0 * zxt * sin(time / 15.0) + zyt * cos(time / 15.0);

  //z *= mat2(cos(time / 17000.0), sin(time / 17000.0), -1.0 * sin(time / 17000.0), cos(time / 17000.0));
  //z.x -= 0.5;
  //tz.x += 0.5;
  tz.x += time / 10.0;
  vec4 res = get_frame(tz);

  //float c = 1.1;
  //  for(float i=1.0; i<= 250.0; i++)
  //c = c / i;

  float s, c;
  s = sin(z.y);
  c = cos(z.y);
  z = vec2(sinh(z.x) * c, cosh(z.x) * s);
  z *= vec2(1.1, 1.1);

  z = torus_reduce(z);

  z = (z + vec2(1.0, 1.0)) / vec2(2.0, 2.0);

  float one = 1.0 / kernel_dim;
  vec4 frame = texture2D(u_image, z);
  /*frame += texture2D(u_image, z + vec2(one, -1.0 * one));
  frame += texture2D(u_image, z + vec2(-1.0 * one, -1.0 * one));
  frame += texture2D(u_image, z + vec2(-1.0 * one, one));
  frame += texture2D(u_image, z + vec2(one, one));

  frame /= vec4(5.0, 5.0, 5.0, 5.0);
  */

  // blend
  res = vec4(res.a * res.rgb + (1.0 - res.a) * frame.rgb, res.a);

  // color
  vec3 color = rgb2hsv(res.rgb);
  color.r += 0.17;

  //res = vec4(z.x, z.y, 0,1);

  gl_FragColor = vec4(hsv2rgb(color), 1.0);
}

vec4 _gamma3(vec4 v, float gamma){
  return vec4(pow(v.xyz, vec3(gamma, gamma, gamma)), v.w);
}

vec4 post(vec4 v){
  v = _gamma3(v, 2.0 / 1.5);

  v = vec4(rgb2hsv(v.rgb), v.a);

  vec3 c0 = hsv2rgb(vec3(0.33, 1.0, 0.5));
  vec3 c1 = hsv2rgb(vec3(0.33 + 0.5 / 2.0, 1.0, 0.5));
  vec3 c2 = hsv2rgb(vec3(0.33 - 0.5 / 2.0, 1.0, -1.0 * 0.5));

  vec3 res, r0, r1;
  float f;

  if(v.x < 1.0 / 3.0){
    f = 3.0 * v.x;
    r0 = c0;
    r1 = c1;
  }else if(v.x < 2.0 / 3.0){
    f = 3.0 * v.x - 1.0;
    r0 = c1;
    r1 = c2;
  }else{
    f = 3.0 * v.x - 2.0;
    r0 = c2;
    r1 = c0;
  }
  res = (1.0 - f) * r0 + f * r1;

  res = rgb2hsv(res.rgb);
  v.x = res.x;

  return vec4(hsv2rgb(v.rgb), v.a);
}
