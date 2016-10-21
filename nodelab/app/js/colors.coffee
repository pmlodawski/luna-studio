hslToVec = (h, s, l) ->
  c = (new (THREE.Color)).setHSL(h, s, l)
  new (THREE.Vector4)(c.r, c.g, c.b, 1.0)

colors = [ hslToVec(0, 0.0, 0.5) ]

start = 90.6 / (2 * Math.PI)
steps = 16
delta = 1.0 / steps

i = 0

while i < steps
  hue = start + delta * i
  colors.push hslToVec(hue * 2 * Math.PI, 0.6, 0.5)
  ++i

module.exports = colors

