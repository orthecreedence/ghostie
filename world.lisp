(in-package :game-level)

(defun create-world () nil)

(defun step-world (world)
  (declare (ignore world)))

(defvar *textures* nil)

(defun load-assets ()
  (format t "Starting asset load.~%")
  (free-assets)
  (let ((trees-png (png-read:read-png-file "resources/trees.png")))
    (let ((gl-tex-trees (car (gl:gen-textures 1))))
      (gl:bind-texture :texture-2d gl-tex-trees)
      (gl:tex-parameter :texture-2d :texture-min-filter :linear)
      ;; these are actually the defaults, just here for reference
      (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
      (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
      (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
      (gl:tex-parameter :texture-2d :texture-border-color '(0 0 0 0))
      (gl:tex-image-2d :texture-2d 0 :rgba
                       (png-read:width trees-png) (png-read:height trees-png)
                       0 :luminance :unsigned-byte
                       (flatten-image-data (png-read:image-data trees-png)))
      (setf (getf *textures* :trees) gl-tex-trees)))
  (format t "Finished asset load.~%"))

(defun free-assets ()
  (gl:delete-textures (loop for (name gl-tex-obj) on *textures* :by #'cddr collect gl-tex-obj)))

(defun draw-world (world)
  (declare (ignore world))
  ;; set up blending
  (when (getf *textures* :trees)
    (gl:enable :texture-2d)
    (gl:bind-texture :texture-2d (getf *textures* :trees)))
  (gl:color 1 1 1)
  (gl:with-primitive :quads
    (gl:tex-coord 0 0)
    (gl:vertex 0 600 0)
    (gl:tex-coord 0 1)
    (gl:vertex  0 0 0)
    (gl:tex-coord 1 1)
    (gl:vertex  2000 0 0)
    (gl:tex-coord 1 0)
    (gl:vertex 2000 600 0))
  (gl:flush))

