(defpackage :ghostie
  (:use :cl :ghostie-config :ghostie-util :ghostie-event)
  (:export #:*game-directory*
           #:*class-path*
           #:*resource-path*
           #:*actor-path*
           #:*level-path*
           #:*object-path*
           #:*compound-object-path*
           #:*log-level*
           #:+dt+

           #:dbg

           #:trigger
           #:bind

           #:recompile-shaders
           
           #:base-object
           ;#:object-position
           ;#:object-rotation
           #:object-physics-body
           #:object-meta
           #:object-display
           #:object-bb
           #:destroy-base-object
           #:calculate-object-bb
           
           #:dynamic-object
           #:object-id
           #:object-level-meta
           #:defobject
           #:load-physics-body
           #:process-object
           #:create-object

           #:compound-object
           #:compound-object-pieces
           #:compound-object-joints
           #:compound-object-motors
           #:compound-object-meta
           #:init-compound-object
           #:create-compound-object

           #:actor
           #:actor-vel-avg-x
           #:actor-vel-avg-y
           #:defactor
           
           #:level
           #:level-objects
           #:level-object
           #:level-meta
           #:add-level-object
           #:remove-level-object
           
           #:world
           #:world-physics
           #:world-position
           #:world-level
           #:world-draw-meta
           
           #:game
           #:game-world
           #:*game*
           #:create-game
           #:stop-game))

