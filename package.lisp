(defpackage :ghostie
  (:use :cl :ghostie-event)
  (:export #:*game-directory*
           #:*class-path*
           #:*resource-path*
           #:*actor-path*
           #:*level-path*

           #:dbg

           #:enqueue

           #:start

           #:game-object
           #:calculate-game-object-bb
           
           #:actor
           #:actor-vel-avg-x
           #:actor-vel-avg-y
           #:load-actor-physics-body
           #:defactor
           #:defgmethod
           
           #:level
           #:world
           #:game
           ))

