// epimorphism utility library


float sinh(float x){
  return (exp(x) - exp(-1.0 * x)) / 2.0;
}

float cosh(float x){
  return (exp(x) + exp(-1.0 * x)) / 2.0;
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

vec4 _gamma3(vec4 v, float gamma){
  return vec4(pow(v.xyz, vec3(gamma, gamma, gamma)), v.w);
}
