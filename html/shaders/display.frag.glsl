precision mediump float;

// our texture
uniform sampler2D u_image;

// the texCoords passed in from the vertex shader.
varying vec2 v_texCoord;

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

vec4 _gamma3(vec4 v, float gamma){
  return vec4(pow(v.xyz, vec3(gamma, gamma, gamma)), v.w);
}

vec4 post(vec4 v){
  v = _gamma3(v, 2.0 / 1.5);

  v = vec4(rgb2hsv(v.rgb), v.a);

  vec3 c0 = hsv2rgb(vec3(0.0, 1.0, 0.5));
  vec3 c1 = hsv2rgb(vec3(0.66 + 0.8 / 2.0, 1.0, 0.5));
  vec3 c2 = hsv2rgb(vec3(0.66 - 0.8 / 2.0, 1.0, -1.0 * 0.5));

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



void main() {
  vec2 clipSpace = gl_FragCoord.xy / vec2(1024.0, 1024.0);
  vec4 res = texture2D(u_image, clipSpace);
  gl_FragColor = post(vec4(res.rgb, 1));
}
