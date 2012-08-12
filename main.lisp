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
  (world-game-cleanup world))

(defun cleanup-render (world)
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

(defun setup-game (world)
  (dbg :info "~%Loading Ghostie~%---------------------~%")
  (load-game-assets world))

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
      ;(dbg :debug "Render queue items: ~a~%" (jpl-queues:size *queue-game-to-render*))
      (enqueue (lambda (game-world) (game-world-sync game-world)) :game)
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
      (progn
        (setf *world* (create-world))
        (setup-game *world*)
        (loop while (not *quit*) do
          ;(dbg :debug "Stepping game!~%")
          (step-game *world*)))
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
    (let ((*world* world))
      (create-window (lambda ()
                       (process-queue world :render)
                       (init-render world))
                     (lambda (dt) (step-render world dt))
                     :title "Ghostie"
                     :width 900
                     :height 600))
    (setf *quit* t))
  (dbg :info "Render thread exit.~%"))

(defun main ()
  (bt:make-thread (lambda ()
                    (stop :force t)
                    (setf *quit* nil)
                    (init-message-queue)
                    (setf *game-thread* (bt:make-thread #'game-thread))
                    (setf *render-thread* (bt:make-thread #'render-thread)))))


