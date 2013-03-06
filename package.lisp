(defpackage :ghostie
  (:use :cl :ghostie-config :ghostie-util :ghostie-event)
  (:export #:*game-directory*
           #:*class-path*
           #:*resource-path*
           #:*actor-path*
           #:*level-path*
           #:*log-level*
           #:+dt+

           #:dbg

           #:enqueue
           #:in-game
           #:in-render

           #:trigger
           #:bind

           #:recompile-shaders
           
           #:game-object
           ;#:game-object-position
           ;#:game-object-rotation
           #:game-object-physics-body
           #:game-object-meta
           #:game-object-display
           #:game-object-bb
           #:calculate-game-object-bb
           
           #:dynamic-object
           #:object-id
           #:object-level-meta
           #:defobject
           #:load-physics-body
           #:process-object
           #:create-object

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
           #:game-game-world
           #:game-render-world
           #:*game*
           #:create-game
           #:stop-game))

