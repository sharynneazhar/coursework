# PUBG Cargo Air Drops

### Change Log
* From Project 3 Review:
  * Fix "overly dark" lighting to be brighter
  * Implement `vHat` for oblique projections
  * Detect projection type by looking at the `ec_lds` matrix instead of passing another uniform to the fragment shader
* Added texture mapping feature to:
  * `Ground` element displays grass texture from `images/ground.jpg`
  * `Building` elements displays wall texture from `images/wall.jpg`
  * `Parachute` elements displays cloth texture from `images/parachute.png`
* Moved blue light post to the back of scene
