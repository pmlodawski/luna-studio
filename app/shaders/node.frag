#ifdef GL_ES
precision highp float;
#endif

varying vec2 coords;
uniform float camFactor;
uniform int selected;

void main() {
  vec2 posN = (gl_FragCoord.xy - coords) / camFactor;
  float dist_squared = dot(posN, posN);

  float camFactorDelta = camFactor - 1.0;
  float rimCamFactor  = 1.0;
  float blurCamFactor = 1.0;
  if (camFactor > 1.0) {
    rimCamFactor  += camFactorDelta / 12.0;
    blurCamFactor += camFactorDelta / 2.5;
  } else if (camFactor < 1.0) {
    rimCamFactor  += camFactorDelta / 1.3;
    blurCamFactor += camFactorDelta / 1.16;
  }
  float r4 = 900.0;
  float r3 = r4 -  80.0 / blurCamFactor;
  float r1 = r4 - 310.0 /  rimCamFactor;
  float r2 = r1 +  80.0 / blurCamFactor;

  float d_blur_r34 = 1.0 - (dist_squared - r3) / (r4 - r3);
  float d_blur_r12 = (dist_squared - r1) / (r2 - r1);

  float bg_gray = 0.1;
  float grey = 0.2;
  vec3 color = vec3(grey, grey, grey);
  vec3 selected_color = vec3(0.85, 0.55, 0.1);
  vec3 focused_color = vec3(0.90, 0.40, 0.05);
  if (selected == 1) {
    color = selected_color;
  } else if (selected == 2) {
    color = focused_color;
  }
  vec3 color_inside = vec3(bg_gray, bg_gray, bg_gray);
  vec3 color_outside = vec3(0.0, 0.0, 0.0);

  if (dist_squared < r1) {
    gl_FragColor = vec4(color_inside, 1.0);
  } else if (dist_squared < r2) {
    vec3 mix = color_inside * (1.0 - d_blur_r12) + color * (d_blur_r12);
    gl_FragColor = vec4(mix, 1.0);
  } else if (dist_squared < r3) {
    gl_FragColor = vec4(color, 1.0);
  } else if (dist_squared < r4) {
    gl_FragColor = vec4(color, d_blur_r34);
  } else {
    gl_FragColor = vec4(color_outside, 0.0);
  }
}
