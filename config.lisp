(defpackage :ghostie-config
  (:use :cl)
  (:export #:+log-levels+
           
           #:*ghostie-directory*
           
           #:*game-directory*
           #:*class-path*
           #:*resource-path*
           #:*actor-path*
           #:*level-path*
           #:*object-path*
           #:*compound-object-path*
           
           #:*log-level*
           
           #:+infinity+
           #:+neg-infinity+

           #:+config-graphics-window-x+
           #:+config-graphics-window-y+
           #:+config-graphics-pixels-per-meter+
           
           #:+dt+
           #:*physics-segment-thickness*))
(in-package :ghostie-config)

(defconstant +log-levels+ '(:emerg 0
                            :error 1
                            :warning 2
                            :notice 3
                            :info 4
                            :debug 5))

(setf *random-state* (make-random-state t))

(defparameter *ghostie-directory* (asdf:system-relative-pathname :ghostie #P""))

(defparameter *game-directory* #P"."
  "The directory the game files live in. All other paths are relative to this.")
(defparameter *class-path* "classes"
  "The name of the directory that the classes folder lives under.")
(defparameter *resource-path* "resources"
  "The name of the directory that the resources folder lives under.")
(defparameter *actor-path* "actors"
  "The name of the directory (under the resources dir) that actors load from.")
(defparameter *level-path* "levels"
  "The name of the directory (under the resources dir) that levels load from.")
(defparameter *object-path* "objects"
  "The name of the directory (under the resources dir) that objects load from.")
(defparameter *compound-object-path* "compound-objects"
  "The name of the directory (under the resources dir) that compound objects load from.")

(defparameter *log-level* :debug)

;; define an "infinity" value
(defconstant +infinity+ most-positive-double-float)
(defconstant +neg-infinity+ most-negative-double-float)

;; --------- graphics config
(defconstant +config-graphics-window-x+ 800)
(defconstant +config-graphics-window-y+ 640)
(defconstant +config-graphics-pixels-per-meter+ 4)

;; --------- physics
(defparameter +dt+ (coerce (/ 1 60) chipmunk-wrapper:+physics-precision+))
(defparameter *physics-segment-thickness* 3.0d0)

