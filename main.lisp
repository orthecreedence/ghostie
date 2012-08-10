;; -----------------------------------------------------------------------------
;; TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO
;;
;; Threading:
;;  - sync adding of new game objects
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
(defparameter *game-thread* nil)
(defparameter *render-thread* nil)

(setf *random-state* (make-random-state t))

(define-condition game-quit (error) ())
(defparameter *quit* nil)

(load "config")
(load "util")
(load "sync")
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

(defun cleanup-game (world)
  (dbg :info "Cleaning up game world~%")
  (world-game-cleanup world))

(defun cleanup-render (world)
  (dbg :info "Cleaning up render world~%")
  (world-render-cleanup world)
  (cleanup-opengl))

(defun stop (&key force)
  (setf *quit* t)
  (dolist (thread (list *game-thread* *render-thread*))
    (when (and (bt:threadp thread)
               (bt:thread-alive-p thread))
      (if force
          (bt:destroy-thread thread)
          (bt:join-thread thread))
      (dbg :info "Ghostie thread stopped (:force ~a)~%" force))))

(defun setup-game ()
  (dbg :info "~%Loading Ghostie~%---------------------~%")
  (load-game-assets *world*))

(defun step-game (world)
  (handler-case
    (progn
      (when *quit* (error 'game-quit))
      (process-queue *world* :game)
      (step-game-world world))
    (error (e)
      (cleanup-game world)
      (error e))))

(defun step-render (world dt)
  (handler-case
    (progn
      (key-handler world dt)
      (process-queue world :render)
      (draw-world world))
    (error (e)
      (dbg :error "Uncaught error in render thread: ~a~%" e)
      (setf *quit* t)
      (cleanup-render world)
      (error e))))

(defun game-thread ()
  (unwind-protect
    (handler-case
      (loop while (not *quit*) do
        (step-game *world*))
      (game-quit ()
        (cleanup-game *world*))
      (error (e)
        (dbg :error "Uncaught error in game thread: ~a~%" e)
        (setf *quit* t)
        (error e)))
    (cleanup-game *world*))
  (dbg :info "Game thread exit.~%"))

(defun render-thread ()
  (let ((world (create-world)))
    (create-window (lambda ()
                     (process-queue world :render)
                     (init-render world))
                   (lambda (dt) (step-render world dt))
                   :title "Ghostie"
                   :width 900
                   :height 600)
    (setf *quit* t))
  (dbg :info "Render thread exit.~%"))

(defun main ()
  (bt:make-thread (lambda ()
                    (stop :force t)
                    (setf *world* (create-world))
                    (setf *quit* nil)
                    (init-message-queue)
                    (setup-game)
                    (setf *game-thread* (bt:make-thread #'game-thread))
                    (setf *render-thread* (bt:make-thread #'render-thread)))))


