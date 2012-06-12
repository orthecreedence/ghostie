(let ((packages '(cl-opengl cl-glu lispbuilder-sdl png-read bordeaux-threads split-sequence cl-triangulation)))
  (dolist (pkg packages)
    (ql:quickload pkg)))

(defpackage :game-level
  (:use :cl))
(in-package :game-level)

(defparameter *world* nil)
(defparameter *main-thread* nil)
(defparameter *quit* nil)

(setf *random-state* (make-random-state t))

(load "util")
(load "matrix")
(load "opengl/shaders")
(load "opengl/object")
(load "input")
(load "window")
(load "world")

(defun stop ()
  (when (and *main-thread*
             (bt:threadp *main-thread*))
    (unless (equal *main-thread* (bt:current-thread))
      (when (bt:thread-alive-p *main-thread*)
        (bt:destroy-thread *main-thread*)))
    (setf *main-thread* nil)))

(defun run-app ()
  (setf *world* (create-world))
  (create-window #'window-event-handler
                 :title "game level"
                 ;:background '(.33 .28 .25 1)
                 :background '(1.0 1.0 1.0 1.0)
                 :width 600
                 :height 600)
  (stop))

(defun main ()
  (stop)
  (setf *main-thread* (bt:make-thread #'run-app)))

