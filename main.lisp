(ql:quickload :ghostie)

(defpackage :ghostie
  (:use :cl))
(in-package :ghostie)

(defparameter *world* nil)
(defparameter *main-thread* nil)
(defparameter *quit* nil)

(setf *random-state* (make-random-state t))
(defparameter *quit* nil)

(load "util")
(load "matrix")
(load "opengl/shaders")
(load "opengl/fbo")
(load "opengl/object")
(load "input")
(load "window")
(load "game-classes/game-object")
(load "game-classes/actor")
(load "game-classes/level")
(load "world")

(defun stop ()
  (when (and *main-thread*
             (bt:threadp *main-thread*))
    (unless (equal *main-thread* (bt:current-thread))
      (when (bt:thread-alive-p *main-thread*)
        (bt:destroy-thread *main-thread*)))
    (setf *main-thread* nil)))

(defun setup ()
  (load-assets *world*))

(defun main-loop (dt)
  (step-world *world* dt)
  (draw-world *world* dt))

(defun run-app ()
  (let ((*quit* nil)
        (*world* (create-world)))
    (create-window #'main-loop
                   :title "game level"
                   :background (hex-to-rgb "#262524" :type 'list)
                   ;:background '(.33 .28 .25 1)
                   ;:background '(1 1 1 1)
                   :width 900
                   :height 600))
  (stop))

(defun main ()
  (stop)
  (setf *main-thread* (bt:make-thread #'run-app :name "game-thread")))

