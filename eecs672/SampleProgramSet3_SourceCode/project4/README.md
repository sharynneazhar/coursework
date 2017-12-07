# PUBG Cargo Air Drops

### Change Log
* From Project 3 Review:
  * Fix "overly dark" lighting to be brighter
  * Implement `vHat` for oblique projections
  * Detect projection type by looking at the `ec_lds` matrix instead of passing another uniform to the fragment shader
* Added texture mapping feature to:
  * `Ground` element displays grass texture from `textures/ground.jpg`
  * `Building` elements displays wall texture from `textures/wall.jpg`
  * `Parachute` elements displays cloth texture from `textures/parachute.jpg`
  * `Puddle` elements displays water texture from `textures/water.jpg`
* Added translucent objects:
  * `LightPost` elements displays a translucent casing
  * `Puddle` elements displays translucent water texture
