(in-package :game-level)

(defparameter *world-position* '(-17.19999 -24.00002 -36.000065))
(defvar *perspective-matrix* nil)
(defvar *ortho-matrix* nil)
(defvar *view-matrix* nil)
(defvar *game-data* nil)

(defun create-world ()
  (setf *world-position* '(-17.19999 -24.00002 -36.000065)))

(defun step-world (world dt)
  (declare (ignore world dt)))

(defun load-assets ()
  (format t "Starting asset load.~%")
  (free-assets)
  (let ((assets '((:ground #P"resources/ground.ai" 0)
                  (:ground-background #P"resources/ground-background.ai" -9)
                  (:tree1 #P"resources/tree1.ai" -20.0)
                  (:tree2 #P"resources/tree2.ai" -14.0)
                  (:tree3 #P"resources/tree3.ai" -16.0)
                  (:tree4 #P"resources/tree4.ai" -32.0))))
    (loop for (key file z-offset) in assets do
          (format t "Loading ~a...~%" file)
          (setf (getf *game-data* key)
                (multiple-value-bind (vertices offset) (load-points-from-ai file :precision 2 :center t :scale '(.1 .1 .1))
                  (make-gl-object :data (cl-triangulation:triangulate (coerce vertices 'vector)) :scale '(1 1 1) :position (append (mapcar #'- offset) (list z-offset)))))))
  ;(let ((assets '((:fg1 "resources/level/fg1.png" -47 0 0)
  ;                (:tree1 "resources/level/tree1.png" 476 1528 0)
  ;                (:bg1 "resources/level/bg1.png" -103 61 0)
  ;                (:bg2 "resources/level/bg2.png" -59 1611 0))))
  ;  (loop for (key file x y z) in assets do
  ;    (format t "Loading ~a~%" file)
  ;    (let* ((png (png-read:read-png-file file))
  ;           (w (png-read:width png))
  ;           (h (png-read:height png))
  ;           (data (flatten-image-data (png-read:image-data png)))
  ;           (tex (car (gl:gen-textures 1))))
  ;      (gl:bind-texture :texture-2d tex)
  ;      (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
  ;      (gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)
  ;      (glu:build-2d-mipmaps :texture-2d 4 w h :rgba :unsigned-byte data)
  ;      (let* ((x1 (- (floor (/ w 2))))
  ;             (y1 (- (floor (/ h 2))))
  ;             (x2 (+ w x1))
  ;             (y2 (+ h y1)))
  ;        (setf (getf *game-data* key)
  ;              (make-gl-object :data `(((,x1 ,y1 0) (,x2 ,y1 0) (,x1 ,y2 0))
  ;                                      ((,y2 ,x2 0) (,x2 ,y2 0) (,x2 ,y2 0)))
  ;                              :uv-map #(0 0 1 0 0 1 1 1)
  ;                              :texture tex
  ;                              :position (list x y z)))))))
  (setf (getf *game-data* :quad) (make-gl-object :data '(((-1 -1 0) (1 -1 0) (-1 1 0))
                                                         ((1 -1 0) (1 1 0) (-1 1 0)))
                                                 :uv-map #(0 0 1 0 0 1 1 1)))
  (setf (getf *game-data* :spike) (make-gl-object :data (load-triangles-from-ply #P"resources/spike.ply") :scale '(1 1 1) :position '(0 0 -10)))
  (create-test-primitives)
  (format t "Finished asset load.~%"))

(defun free-assets ()
  (loop for (nil obj) on *game-data* by #'cddr do
    (when (subtypep (type-of obj) 'gl-object)
      (free-gl-object obj))))

(defun draw-world (world dt)
  (declare (ignore world dt))
  (when *quit* (return-from draw-world nil))
  (gl:bind-framebuffer-ext :framebuffer (gl-fbo-fbo (getf *render-objs* :fbo1)))
  (gl:clear :color-buffer-bit :depth-buffer)
  (use-shader :main)
  (set-shader-var #'gl:uniformf "fogAmt" 1.0)
  (setf *view-matrix* (apply #'m-translate *world-position*))
  (set-shader-matrix "cameraToClipMatrix" *perspective-matrix*)
  (draw (getf *game-data* :ground))
  (draw (getf *game-data* :ground-background))
  (draw (getf *game-data* :tree1))
  (draw (getf *game-data* :tree2))
  (draw (getf *game-data* :tree3))
  (draw (getf *game-data* :tree4))
  (gl:bind-framebuffer-ext :framebuffer 0)
  (let ((fbo (getf *render-objs* :fbo1)))
    (gl:clear :color-buffer-bit :depth-buffer-bit)
    (use-shader :dof)
    (set-shader-matrix "cameraToClipMatrix" *ortho-matrix*)
    (set-shader-var #'gl:uniformi "renderTexWidth" 600)
    (set-shader-var #'gl:uniformi "renderTexHeight" 600)
    (gl:active-texture :texture0)
    (gl:bind-texture :texture-2d (gl-fbo-tex fbo))
    (gl:generate-mipmap-ext :texture-2d)
    (set-shader-var #'gl:uniformi "renderTex" 0)
    (gl:active-texture :texture1)
    (gl:bind-texture :texture-2d (gl-fbo-depth fbo))
    (set-shader-var #'gl:uniformi "depthTex" 1))
  (draw (getf *game-data* :quad))
  (use-shader 0))

(defun test-gl-funcs ()
  (gl:clear-color 1 1 1 1)
  (format t "OpenGL version: ~a~%" (gl:get-string :version))
  (format t "Shader version: ~a~%" (gl:get-string :shading-language-version))
  ;(format t "Extensions: ~a~%" (gl:get-string :extensions))
  (format t "Err: ~a~%" (gl:get-error)))

(defun create-test-primitives ()
  (setf (getf *game-data* :triangle) (make-gl-object :data '(((-1.0 -1.0  0.0) ( 1.0 -1.0  0.0) ( 0.0  1.0  0.0))) :position '(0 0 -1)))
  (setf (getf *game-data* :prism1) (make-gl-object :data '(((-1.0 -1.0  0.0) ( 1.0 -1.0  0.0) ( 0.0  1.0  0.0))
                                                           ((-1.0 -1.0  0.0) ( 0.0  0.0 -1.0) ( 1.0 -1.0  0.0))
                                                           ((-1.0 -1.0  0.0) ( 0.0  1.0  0.0) ( 0.0  0.0 -1.0))
                                                           (( 0.0  1.0  0.0) ( 1.0 -1.0  0.0) ( 0.0  0.0 -1.0))) :position '(3 6 -20)))
  (setf (getf *game-data* :prism2) (make-gl-object :data '(((-1.0 -1.0  0.0) ( 1.0 -1.0  0.0) ( 0.0  1.0  0.0))
                                                           ((-1.0 -1.0  0.0) ( 0.0  0.0 -1.0) ( 1.0 -1.0  0.0))
                                                           ((-1.0 -1.0  0.0) ( 0.0  1.0  0.0) ( 0.0  0.0 -1.0))
                                                           (( 0.0  1.0  0.0) ( 1.0 -1.0  0.0) ( 0.0  0.0 -1.0))) :position '(-3 4 -30)))
  (setf (getf *game-data* :prism3) (make-gl-object :data '(((-1.0 -1.0  0.0) ( 1.0 -1.0  0.0) ( 0.0  1.0  0.0))
                                                           ((-1.0 -1.0  0.0) ( 0.0  0.0 -1.0) ( 1.0 -1.0  0.0))
                                                           ((-1.0 -1.0  0.0) ( 0.0  1.0  0.0) ( 0.0  0.0 -1.0))
                                                           (( 0.0  1.0  0.0) ( 1.0 -1.0  0.0) ( 0.0  0.0 -1.0))) :position '(-1 -4 -25))))
