(in-package :game-level)

(defclass game-object ()
  ((name :accessor game-object-name :initform "game-object")
   (position :accessor game-object-position :initarg :position :initform '(0 0 0))
   (gl-objects :accessor game-object-gl-objects)
   (physics :accessor game-object-physics)))

(defun create-game-object (&key gl-objects physics (position '(0 0 0)))
  (let ((obj (make-instance 'game-object :position position)))
    (setf (game-object-gl-objects obj) gl-objects
          (game-object-physics obj) physics)
    obj))

(defmethod draw ((object game-object))
  (dolist (gl-object (game-object-gl-objects object))
    (draw-gl-object gl-object :position (game-object-position object))))
