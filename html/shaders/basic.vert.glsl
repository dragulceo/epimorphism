attribute vec2 a_position;

uniform float u_flipY;
uniform float kernel_dim;

void main() {

  // convert the rectangle from pixels to 0.0 to 1.0
  vec2 clipSpace = 2.0 * a_position / vec2(kernel_dim, kernel_dim) - 1.0;

  gl_Position = vec4(clipSpace, 0, 1);
}
