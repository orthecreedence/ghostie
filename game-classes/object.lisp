(in-package :game-level)

(defclass game-object ()
  ((gl :accessor game-object-gl)
   (physics :accessor game-object-physics)))

(defun create-game-object (&key gl physics)
  (let ((obj (make-instance 'game-object)))
    (setf (game-object-gl obj) gl
          (game-object-physics obj) physics)
    obj))
