(in-package :ghostie)

(defvar *game* nil
  "Holds the main game object, created by create-game.")

(defclass game ()
  ((world :accessor game-world :initarg :world :initform nil)
   (thread :accessor game-thread :initarg :thread :initform nil)
   (quit :accessor game-quit :initform nil)
   (event-bindings :accessor game-event-bindings :initarg :event-bindings :initform nil)
   (loaded-objects :accessor game-loaded-objects :initarg :loaded-objects :initform (make-hash-table :test #'equal)))
  (:documentation
   "Holds a world and any corresponding objects."))

(defun create-game (level-name setup-fn)
  "Create a game, and its two accompanying worlds (game and render)."
  (let ((world (create-world))
        (game nil))  ; placeholder
    (flet ((game-thread ()
             ;; some thread-locs, brah. LOCALS ONLLLYYY
             (let ((*quit* nil)
                   (*game* game)
                   (*event-bindings* (game-event-bindings game)))
               (unwind-protect
                 (progn
                   (funcall setup-fn game)
                   (dbg :info "(game) Starting game thread~%")
                   (create-window (lambda ()
                                    (dbg :info "(game) Initializing render platform~%")
                                    (init-render world)
                                    (world-load-level world level-name))
                                  (lambda (dt)
                                    (step-world world dt))
                                  (lambda ()
                                    (world-cleanup world)
                                    (cleanup-opengl))
                                  :title "Ghostie"
                                  :width 900
                                  :height 600))
                 (setf (game-quit game) t)
                 (trigger :exit game)
                 (dbg :info "(game) Game thread exiting~%")))))
      (dbg :info "(game) Creating game object~%")
      (setf game (make-instance 'game
                   :world world
                   :event-bindings (make-instance 'event-bindings)))
      ;; set this separate so the thread has access to the `game` var for its
      ;; thread-local binding
      (setf (game-thread game) (bt:make-thread #'game-thread :name "game-thread"))
      game)))

(defun stop-game (game &key force)
  "Stop a game, either cleanly or force it."
  (dbg :info "(game) Stopping game thread~%")
  (setf (game-quit game) t
        *quit* t))

