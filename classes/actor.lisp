(in-package :ghostie)
;THE END.

(defclass actor (dynamic-object)
  ((vel-avg-x :accessor actor-vel-avg-x :initform 0d0)
   (vel-avg-y :accessor actor-vel-avg-y :initform 0d0)))

(defmacro defactor (class-name superclasses slots &rest class-options)
  "Abstraction of defclass, solves inter-package issues (in other words, allows
   a game to add its own actor class, and allows ghostie to see it in its
   namespace by importing it)."
  ;; just simple wrap around defobject
  `(defobject ,class-name ,superclasses ,slots ,@class-options))

(defun update-actor-state (actor)
  (when (and actor (game-object-physics-body actor))
    (let ((alpha 1/1000)
          (vel-x (cp-a:body-v-x (cpw:base-c (game-object-physics-body actor))))
          (vel-y (cp-a:body-v-y (cpw:base-c (game-object-physics-body actor)))))
      ;(dbg :debug "avg-y: ~s~%" (+ (* alpha vel-y) (* (- 1d0 alpha) (actor-vel-avg-y actor))))
      (setf (actor-vel-avg-x actor) (+ (* alpha vel-x)
                                       (* (- 1d0 alpha) (actor-vel-avg-x actor)))
            (actor-vel-avg-y actor) (+ (* alpha vel-y)
                                       (* (- 1d0 alpha) (actor-vel-avg-y actor)))))))

