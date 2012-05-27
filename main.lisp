(defvar *pkg-loaded* nil)
(unless *pkg-loaded*
  (let ((packages '(cl-opengl cl-glu lispbuilder-sdl png-read bordeaux-threads split-sequence)))
    (dolist (pkg packages)
      (ql:quickload pkg)))
  (setf *pkg-loaded* t))

(defpackage :game-level
  (:use :cl))
(in-package :game-level)

(defparameter *world* nil)
(defparameter *main-thread* nil)

(setf *random-state* (make-random-state t))

(load "util")
(load "window")
(load "world")

(defun window-event-handler (w)
  (declare (ignore w))
  (sdl:with-events (:poll)
    (:quit-event () t)
    (:video-expose-event () (sdl:update-display))
    (:key-down-event (:key key)
      (when (sdl:key= key :sdl-key-l)
        (load-assets))
      (when (sdl:key= key :sdl-key-q)
        (sdl:push-quit-event))
      (when (sdl:key= key :sdl-key-escape)
        (sdl:push-quit-event)))
    (:idle ()
      (step-world *world*)
      (gl:enable :blend)
      (gl:blend-func :src-alpha :one-minus-src-alpha)
      (gl:clear :color-buffer :depth-buffer)
      (draw-world *world*)
      (sdl:update-display))))


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

(defun stop ()
  (when (and *main-thread*
             (bt:threadp *main-thread*))
    (unless (equal *main-thread* (bt:current-thread))
      (when (bt:thread-alive-p *main-thread*)
        (bt:destroy-thread *main-thread*)))
    (setf *main-thread* nil)))

