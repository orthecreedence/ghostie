(in-package :game-level)

(defclass actor ()
  ((gl-objects :accessor actor-gl-objects :initform nil)))

(defun parse-actor-file (file)
  (declare (ignore file))
  nil)
