;; -----------------------------------------------------------------------------
;; TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO
;;
;; Threading:
;;  - completely init the game thread before doing the render thread. this way
;;    the data is waiting for render when it comes up.
;;  - have to create GL-OBJECTs *in* the render thread since they rely on the
;;    OpenGL context. shit.
;;  - when a level is created, create a blank level in the render thread (just
;;    to hold the game objects)
;;  - when a game object is created, send it (along with its gl objects) to the
;;    render thread, then *get rid of* the gl objects, and send any mods to them
;;    as functions (need some way to reference objects by id)
;;  - for each game loop, if a game object's position/rotation change at all (or
;;    anything that affects drawing) send a diff function onto the render queue.
;;    this should only happen for diffs!
;;  - when the world position changes in render, sync it to game.
;;
;; TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO
;; -----------------------------------------------------------------------------

(ql:quickload :ghostie)

(defpackage :ghostie
  (:use :cl))
(in-package :ghostie)

(defparameter *world* nil)
(defparameter *main-thread* nil)
(defparameter *game-thread* nil)

(setf *random-state* (make-random-state t))

(define-condition game-quit (error) ())
(defparameter *quit* nil)

(load "util")
(load "sync")
(load "config")
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
(load "physics")

(defun cleanup ()
  (format t "Cleaning up.~%")
  (world-cleanup *world*)
  (cleanup-opengl))

(defun stop (&key force)
  (dolist (thread (list *game-thread* *main-thread*))
    (when (bt:threadp thread)
      (if force
          (bt:destroy-thread thread)
          (when (bt:thread-alive-p thread)
            (bt:join-thread thread)))))
  (setf *main-thread* nil
        *game-thread* nil))

(defun setup ()
  (load-assets *world*))

(defun step-game (world)
  (process-queue *world* :game)
  (step-world world))

(defun step-render (world)
  (process-queue world :render)
  (draw-world world))

(defun game-thread ()
  (setup)
  (handler-case
    (loop
      (step-game *world*))
    (game-quit ()
      (cleanup))
    (error (e)
      (format t "Uncaught error in game thread: ~a~%" e))))

(defun render-thread ()
  (let ((world (create-world)))
    (create-window (lambda () (init-render))
                   (lambda () (step-render world))
                   :title "Ghostie"
                   :width 900
                   :height 600)))

(defun run-app ()
  (setf *world* (create-world))
  (let ((*quit* nil))
    (unwind-protect
      (progn
        (init-message-queue)
        (setf *game-thread* (bt:make-thread #'game-thread))
        (render-thread))
      (stop))))

(defun main ()
  (stop)
  (setf *main-thread* (bt:make-thread #'run-app :name "game-thread")))

