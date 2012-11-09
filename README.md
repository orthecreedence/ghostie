Ghostie
=======
Ghostie is a platformer engine for Common Lisp.

At the moment, it is not so much an engine, as an engine with a game integrated.
It uses chipmunk physics for collision detection and SVG files for levels.

Ghostie is a joint effort between my friend Andy and myself.

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
You will need to have [Chipmunk physics](http://chipmunk-physics.net/) >= 6 and
[GLFW](http://www.glfw.org/) installed on your system for Ghostie to run.


Usage
-----
```common-lisp
(load "main")
(ghostie::main)
```

Key bindings:

- Arrow keys: move character
- `a`: Add box to simulation (for testing physics)
- `l`: Reload level from scratch (reset player position, remove any boxes, etc)
- `-`: Zoom out camera
- `=`: Zoom in camera
- `c`: Recompile shaders (allows modification of shaders in-game)
- `q`: Quit

Right now, all you can really do is load specific levels (see world.lisp
`load-game-assets`) and jump around. The `level-object` branch has a
half-working implementation of moving platforms.

TODO
----
- __Dynamic objects.__ Right now, there are level objects (basically anything
that shows up on the 0 z index: ground, fixed platforms, etc) and "actors" as
I call them in the engine ("character" is reserved...). In the `level-object`
branch, there is an implementation of a moving platform. I'd like to add in
more stock dynamic objects like this and make them scriptable.
- __Level editor.__ SVG files are great for laying out the design of a level,
but currently all objects that are not strictly part of the scenery/terrain are
described in a level meta file. This works great, but editing these files could
get horrendously tedious. Having a simple level editor that lets you choose
initial actor positions, dynamic objects (moving platform here, wind-swept grass
there, jointed bridge across here, etc) would make creating new levels and
scenarios a breeze.
- __Scriptability.__ When the main actor does A, B happens (outside of physics
simulation). Being able to script different scenarios in levels would be both
powerful and easy (if scripted in lisp).
- __Backgrounds.__ Right now, there's no way to have a non-moving backdrop for
your level. You can add a layer that's really far back, but it will still move
as the character changes position. Would be nice to have a fixed background
layer that everything is rendered on top of so something like the sun/moon
could be accurately expressed.
- __More stock shaders.__ I want to add in some stock effects that a level can
easily implement to look prettier. Things like film grain, lighting effects,
particle effects, depth of field, bloom, etc etc etc.

Notes
-----
Ghostie *requires* OpenGL >= 3.3 as it makes use of shaders and some other newer
OpenGL extensions that are only available in later versions. Get with the times,
man.

Ghostie is entirely a work in progress and there is a lot of non-trivial work to
do on it, as outlined above. My goal is to not only provide a viable option for
platform game programming in lisp, but make it so easy that armed with a vision
and some time, anybody can make a good looking, fun to play game in the best
programming language available.

