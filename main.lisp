(let ((packages '(cl-glfw cl-opengl cl-glu png-read bordeaux-threads split-sequence cl-triangulation)))
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

(defun key-handler (key action)
  (when (eql action glfw:+press+)
    (case key
      (:minus 
        (decf (nth 2 *world-position*)))
      (:equals
        (incf (nth 2 *world-position*)))
      (:up
        (decf (nth 1 *world-position*) .2))
      (:down
        (incf (nth 1 *world-position*) .2))
      (:left
        (incf (nth 0 *world-position*) .2))
      (:right
        (decf (nth 0 *world-position*) .2))
      (#\R
       (setf *world-position* '(0 0 -1))) ;'(-265 -435 -256)
      (#\T
        (test-gl-funcs))
      (#\S
        (let ((program (gl:create-program)))
          (format t "Prog: ~a~%" program)
          (gl:delete-program program)))
      (#\V
        (format t "ver: ~A~%" (gl:get-string :version)))
      (#\L
        (load-assets))
      (#\Q
        (glfw:close-window))
      (:esc
        (glfw:close-window)))))

(defun window-event-handler ()
  (step-world *world*)
  ;(draw-world *world*)
  (glfw:swap-buffers))

#|
(defun window-event-handler_ (w)
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
|#

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

