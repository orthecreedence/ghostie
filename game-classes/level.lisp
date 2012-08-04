(in-package :ghostie)

(defparameter *level-directory* "resources/levels")

(defclass level ()
  ((objects :accessor level-objects :initform nil)
   (main-actor :accessor level-main-actor :initform nil)
   (actors :accessor level-actors :initform nil)
   (collision-depth :accessor level-collision-depth :initform 0)))

(defun load-level (level-name)
  "Load the level-name level! Does this by loading the SVG file holding the
  objects for the level, and the associated meta file that describes the scene
  and the actors in the level."
  (let ((level (make-instance 'level))
        (objects (svgp:parse-svg-file (format nil "~a/~a/objects.svg" *level-directory* level-name)
                                      :curve-resolution 20
                                      :invert-y t
                                      :ignore-errors t))
        (level-meta (read-file (format nil "~a/~a/meta.lisp" *level-directory* level-name))))
    (setf (level-objects level) (svg-to-game-objects objects level-meta :scale (getf level-meta :scale) :center-objects t)
          (level-actors level) (load-actors (getf level-meta :actors) :scale (getf level-meta :scale)))
    (setf (level-main-actor level) (find-if (lambda (actor) (actor-is-main actor))
                                            (level-actors level)))
    level))

(defun draw-level (level)
  "...draw the entire level..."
  (dolist (game-obj (level-objects level))
    (let ((body (game-object-physics-body game-obj)))
      (when body
        (sync-game-object-to-physics game-obj)))
    (draw game-obj))
  (dolist (actor (level-actors level))
    (draw actor)))

