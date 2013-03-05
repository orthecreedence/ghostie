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

