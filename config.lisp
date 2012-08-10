(in-package :ghostie)

(defparameter *log-level* :debug)

;; define an "infinity" value
(defconstant +infinity-f+ most-positive-single-float)
(defconstant +neg-infinity-f+ most-negative-single-float)

;; --------- graphics config
(defconstant +config-graphics-window-x+ 800)
(defconstant +config-graphics-window-y+ 640)
(defconstant +config-graphics-pixels-per-meter+ 4)

;; --------- physics
