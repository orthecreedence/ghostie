(in-package :game-level)

(defun create-world () nil)

(defun step-world (world)
  (declare (ignore world)))

(defvar *textures* nil)
(defvar *game-data* nil)

(defun load-assets ()
  (format t "Starting asset load.~%")
  (free-assets)
  (let ((ground-data (load-points-from-ai #P"resources/ground.ai")))
    (setf (getf *game-data* :ground-data) ground-data))
  (format t "Finished asset load.~%"))

(defun free-assets ()
  (let ((textures (loop for (name gl-tex-obj) on *textures* :by #'cddr collect gl-tex-obj)))
    (when textures (gl:delete-textures textures))))

(defun draw-world (world)
  (declare (ignore world))
  ;; set up blending
  (gl:color 0 0 0)
  (gl:push-matrix)
  (gl:translate 0 0 0)
  (gl:with-primitive :polygon
    (dolist (vert (getf *game-data* :ground-data))
      (let ((x (car vert))
            (y (cadr vert)))
        (gl:vertex x y 0))))
  (gl:pop-matrix)
  (gl:translate 0 0 0)
  (gl:flush))

