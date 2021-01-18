uniform float EffectStrength;

void main (void)
{
  float eff = EffectStrength * (1.0 + (sin(screenf_x() / 19.0) + sin(screenf_x() / 37.0) + 0.5 * sin(screenf_x() / 7.0) + 0.5 * sin(screenf_x() / 98.0) + 3.0) / 3.0);
  if (screenf_y() < float(screen_height) - eff)
  {
    gl_FragColor = screenf_get_color(vec2(screenf_x(), screenf_y() + eff));
  }
  else
  {
    discard;
  }
}
