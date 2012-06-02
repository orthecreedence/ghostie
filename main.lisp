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
(load "opengl/shaders")
(load "opengl/util")
(load "opengl/object")
(load "window")
(load "world")

(defun key-handler (key)
  (when (sdl:key= key :sdl-key-minus)
    (decf (nth 2 *world-position*)))
  (when (sdl:key= key :sdl-key-equals)
    (incf (nth 2 *world-position*)))
  (when (sdl:key= key :sdl-key-up)
    (decf (nth 1 *world-position*) 2))
  (when (sdl:key= key :sdl-key-down)
    (incf (nth 1 *world-position*) 2))
  (when (sdl:key= key :sdl-key-left)
    (incf (nth 0 *world-position*) 2))
  (when (sdl:key= key :sdl-key-right)
    (decf (nth 0 *world-position*) 2))
  (when (sdl:key= key :sdl-key-r)
    (setf *world-position* '(0 0 -1))) ;'(-265 -435 -256)))
  (when (sdl:key= key :sdl-key-t)
    (test-gl-funcs))
  (when (sdl:key= key :sdl-key-l)
    (load-assets))
  (when (sdl:key= key :sdl-key-q)
    (sdl:push-quit-event))
  (when (sdl:key= key :sdl-key-escape)
    (sdl:push-quit-event)))

(defun window-event-handler (w)
  (declare (ignore w))
  (load-assets)
  (sdl:with-events (:poll)
    (:quit-event () t)
    (:video-expose-event () (sdl:update-display))
    (:video-resize-event (:w width :h height)
      (resize-window width height))
    (:key-down-event (:key key)
      (key-handler key))
    (:idle ()
      (step-world *world*)
      (draw-world *world*)
      (sdl:update-display))))

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
                 :width 800
                 :height 600
                 :background '(1 1 1 0))
  (stop))

(defun main ()
  (unless *main-thread*
    (setf *main-thread* (bt:make-thread #'run-app))))

