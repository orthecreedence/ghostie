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
    (setf (level-objects level) (svg-to-game-objects objects level-meta)
          (level-actors level) (load-actors (getf level-meta :actors)))
    level))

(defun svg-to-game-objects (objects level-meta)
  (let ((obj-hash (make-hash-table :test #'equal))
        (game-objects nil))
    (loop for i from 0
          for obj in objects do
      (when (< 0 (length (getf obj :point-data)))
        (let* ((opacity (if (getf obj :opacity) (getf obj :opacity) 1))
               (color (if (getf obj :fill)
                          (hex-to-rgb (getf obj :fill) :opacity opacity)
                          (vector 0 0 0 opacity)))
               (triangles (glu-tessellate:tessellate (getf obj :point-data)))
               (group-name (car (getf obj :group))))
          (when (> (length triangles) 0)
            (push (make-gl-object :data triangles :color color :scale (getf level-meta :scale))
                  (gethash group-name obj-hash))))))
    (loop for group-name being the hash-keys of obj-hash
          for gl-objects being the hash-values of obj-hash do
      (let* ((meta (find-if (lambda (p) (equal (getf p :name) group-name)) (getf level-meta :object-properties)))
             (depth (getf meta :layer-depth)))
        (format t "meta: ~a~%" meta)
        (push (create-game-object :gl-objects gl-objects
                                  :physics nil
                                  :position (list 0 0 (if depth depth 0)))
              game-objects)))
    game-objects))


(defun draw-level (level)
  (dolist (game-obj (level-objects level))
    (draw game-obj))
  (dolist (actor (level-actors level))
    (draw actor)))

