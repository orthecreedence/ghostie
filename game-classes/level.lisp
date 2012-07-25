(in-package :game-level)

(defparameter *level-directory* "resources/levels")

(defclass level ()
  ((objects :accessor level-objects :initform nil)
   (actors :accessor level-actors :initform nil)
   (collision-depth :accessor level-collision-depth :initform 0)))

(defun load-level (level-name)
  (let ((level (make-instance 'level))
        (objects (svgp:parse-svg-file (format nil "~a/~a/objects.svg" *level-directory* level-name)
                                      :curve-resolution 20
                                      :invert-y t
                                      :ignore-errors t))
        (level-meta (read-file (format nil "~a/~a/meta.lisp" *level-directory* level-name))))
    (setf (level-objects level) (svg-to-game-objects objects level-meta :scale (getf level-meta :scale))
          (level-actors level) (load-actors (getf level-meta :actors) :scale (getf level-meta :scale)))
    level))

(defun draw-level (level)
  (dolist (game-obj (level-objects level))
    (draw game-obj))
  (dolist (actor (level-actors level))
    (draw actor)))

