(in-package :ghostie)

(defvar *game* nil
  "Holds the main game object, created by create-game.")

(defclass game ()
  ((game-world :accessor game-game-world :initarg :game-world :initform nil)
   (game-thread :accessor game-game-thread :initarg :game-thread :initform nil)
   (render-world :accessor game-render-world :initarg :render-world :initform nil)
   (render-thread :accessor game-render-thread :initarg :render-thread :initform nil)
   (quit :accessor game-quit :initform nil)
   (event-bindings :accessor game-event-bindings :initarg :event-bindings :initform nil)
   (loaded-objects :accessor game-loaded-objects :initarg :loaded-objects :initform (make-hash-table :test #'equal)))
  (:documentation
   "Holds a game world, a render world, and their corresponding threads."))

(defun create-game (level-name)
  "Create a game, and its two accompanying worlds (game and render)."
  (let ((game-world (create-world))
        (render-world (create-world))
        (game nil))  ; placeholder
    (setf *quit* nil)
    (flet ((game-thread ()
             (unwind-protect
               (progn
                 (dbg :info "(game) Starting game thread~%")
                 (world-load-level game-world level-name)
                 (loop while (not (game-quit game)) do
                   (process-queue game-world :game)
                   (step-game-world game-world)))
               (world-game-cleanup game-world)
               (trigger :game-exit game)
               (dbg :info "(game) Game thread exiting~%")))
           (render-thread ()
             (dbg :info "(game) Starting render thread~%")
             (create-window (lambda ()
                              (init-render render-world)
                              (process-queue render-world :render))
                            (lambda (dt) (step-render-world render-world dt))
                            (lambda ()
                              (world-render-cleanup render-world)
                              (cleanup-opengl))
                            :title "Ghostie"
                            :width 900
                            :height 600)
             (setf (game-quit game) t)
             (trigger :render-exit game)
             (dbg :info "(game) Render thread exiting~%")))
      (init-message-queue)
      (dbg :info "(game) Creating game object~%")
      (setf game (make-instance 'game
                   :game-world game-world
                   :game-thread (bt:make-thread #'game-thread :name "game-thread")
                   :render-world render-world
                   :render-thread (bt:make-thread #'render-thread :name "render-thread")
                   :event-bindings (make-instance 'event-bindings)))
      (unless *game*
        (setf *game* game)
        (setf *event-bindings* (game-event-bindings game)))
      game)))

(defun stop-game (game &key force)
  "Stop a game, either cleanly or force it."
  (setf (game-quit game) t)
  (flet ((do-quit ()
           (world-game-cleanup (game-game-world game))
           (dolist (thread (list (game-game-thread game) (game-render-thread game)))
             (when (and (bt:threadp thread)
                        (bt:thread-alive-p thread))
               (if force
                   (bt:destroy-thread thread)
                   (bt:join-thread thread))
               (dbg :info "Ghostie thread stopped (:force ~a)~%" force)))
           (when (equal *game* game)
             (setf *game* nil
                   *event-bindings* nil))))
    (dbg :info "(game) Stopping game and render threads~%")
    (in-render ()
      (setf *quit* t)
      (in-game ()
        (do-quit)))))
