Ghostie
=======
Ghostie is a 2D game engine engine for Common Lisp.
[See the demo here in low-res with shitty colors (anyone know a good screencasting tool for windows?)](https://vimeo.com/61212217).

The goal of Ghostie is to provide an engine for easily creating beautiful games
without making (m)any assumptions about the gameplay itself.

Dependencies
------------
Some of these probably aren't in Quicklisp, so you'll have to put them into your
`local-projects/`. Sorry.

- [cl-svg-polygon](https://github.com/orthecreedence/cl-svg-polygon) - Used to
convert SVG files into levels.
- [glu-tessellate](https://github.com/orthecreedence/glu-tessellate) - Used to
transform polygons into triangles (for OpenGL rendering).
- [clipmunk](https://github.com/orthecreedence/clipmunk) - My fork of the
clipmunk bindings, which wrap the latest versions of the Chipmunk physics
library.
- [chipmunk-wrapper](https://github.com/orthecreedence/chipmunk-wrapper) - Some
nice CLOS wrappage around `clipmunk` that makes the bindings a bit more lispy.

#### C Libraries
You will need to have [Chipmunk physics](http://chipmunk-physics.net/) >= 6.1.3
(compiled with `-DCHIPMUNK_FFI`) and [GLFW](http://www.glfw.org/) installed on
your system for Ghostie to run.


Usage
-----
```common-lisp
(ql:quickload :ghostie-demo)
(ghostie-demo:start)
```

This starts the demo level, which is a basic demonstration of a mobile character
and interaction with the physics environment. The demo level is tiny for now,
but the idea is eventually to create a full-blown game level in the demo.

Key bindings:

- Arrow keys: move character
- `a`: Add box to simulation (for testing physics)
- `l`: Reload level from scratch (reset player position, remove any boxes, etc)
- `-`: Zoom out camera
- `=`: Zoom in camera
- `c`: Recompile shaders (allows modification of shaders in-game)
- `q`: Quit

TODO
----
This is constantly changing as I figure out what I want to do with Ghostie.
For a complete TODO, [see the issues list](https://github.com/orthecreedence/ghostie/issues).

Notes
-----
Ghostie *requires* OpenGL >= 3.3 as it makes use of shaders and some other newer
OpenGL extensions that are only available in later versions. Get with the times,
man.

Ghostie *currently* runs everything (game logic/physics/rendering) in one
thread. Control is immediately returned to the caller after the thread is
spawned.

Ghostie is entirely a work in progress and there is a lot of non-trivial work to
do on it, as outlined above. My goal is to not only provide a viable option for
2D game programming in lisp, but make it so easy that armed with a vision and
some time, anybody can make a good looking, fun to play game in the best
programming language available.

