(in-package :ghostie)

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

(defparameter *log-level* :debug)

;; define an "infinity" value
(defconstant +infinity-f+ most-positive-single-float)
(defconstant +neg-infinity-f+ most-negative-single-float)

;; --------- graphics config
(defconstant +config-graphics-window-x+ 800)
(defconstant +config-graphics-window-y+ 640)
(defconstant +config-graphics-pixels-per-meter+ 4)

;; --------- physics
(defparameter +dt+ (coerce (/ 1 60) chipmunk-wrapper:+physics-precision+))
(defparameter *physics-segment-thickness* 2.0d0)
(defparameter *character-max-vel* 1000d0)
(defparameter *character-max-run* 200)
