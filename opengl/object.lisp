(in-package :ghostie)

(defclass gl-object ()
  ((vertex-data :accessor gl-object-vertex-data :initarg :vertex-data :initform nil)
   (index-data :accessor gl-object-index-data :initarg :index-data :initform nil)
   (scale :accessor gl-object-scale :initarg :scale :initform '(1 1 1))
   (position :accessor gl-object-position :initarg :position :initform '(0 0 0))
   (rotation :accessor gl-object-rotation :initarg :rotation :initform '(0 0 0 0))
   (texture :accessor gl-object-texture :initform nil)
   (color :accessor gl-object-color :initarg :color :initform #(0 0 0 1))
   (vao :accessor gl-object-vao :initform nil)
   (vbos :accessor gl-object-vbos :initform nil)))

(defun make-gl-object (&key data (position '(0 0 -1)) (scale '(1 1 1)) texture uv-map (color #(0 0 0 1)))
  (set-gl-object-data (make-instance 'gl-object :position position :scale scale :color color) data texture uv-map))

(defmethod set-gl-object-data (gl-object (triangles list) &optional texture uv-map)
  "Copies a set of floating-point vertex data into a VBO which is then stored
  with the gl-object."
  (free-gl-object gl-object)
  (multiple-value-bind (vertices index) (create-point-index triangles)
    (let* ((vertex-array (flatten-vertices-into-array vertices))
           (buffers (gl:gen-buffers 2))
           (vertex-buffer (car buffers))
           (index-buffer (cadr buffers)))

      ;; set up vertex buffer
      (gl:bind-buffer :array-buffer vertex-buffer)
      (let ((gl-arr (gl:alloc-gl-array :float (length vertex-array))))
        (dotimes (i (length vertex-array))
          (setf (gl:glaref gl-arr i) (coerce (aref vertex-array i) 'single-float)))
        (gl:buffer-data :array-buffer :static-draw gl-arr)
        (gl:free-gl-array gl-arr))
      (gl:bind-buffer :array-buffer 0)

      ;; set up texture map if provided
      (when uv-map
        (let ((vbo-uv (car (gl:gen-buffers 1))))
          (gl:bind-buffer :array-buffer vbo-uv)
          (let ((gl-arr (gl:alloc-gl-array :float (length uv-map))))
            (dotimes (i (length uv-map))
              (setf (gl:glaref gl-arr i) (coerce (aref uv-map i) 'single-float)))
            (gl:buffer-data :array-buffer :static-draw gl-arr)
            (gl:free-gl-array gl-arr))
          (gl:bind-buffer :array-buffer 0)
          (setf (getf (gl-object-vbos gl-object) :uv) vbo-uv)))

      ; set up index buffer
      (gl:bind-buffer :element-array-buffer index-buffer)
      (let ((gl-arr (gl:alloc-gl-array :unsigned-short (length index))))
        (dotimes (i (length index))
          (setf (gl:glaref gl-arr i) (aref index i)))
        (gl:buffer-data :element-array-buffer :static-draw gl-arr)
        (gl:free-gl-array gl-arr))
      (gl:bind-buffer :element-array-buffer 0)

      ; set up the vertex array with the appropriate data
      (setf (gl-object-vao gl-object) (gl:gen-vertex-array))
      (gl:bind-vertex-array (gl-object-vao gl-object))
      (gl:bind-buffer :array-buffer vertex-buffer)
      (gl:enable-vertex-attrib-array 0)
      (gl:vertex-attrib-pointer 0 3 :float nil 0 (cffi:null-pointer))
      (when uv-map
        (gl:bind-buffer :array-buffer (getf (gl-object-vbos gl-object) :uv))
        (gl:enable-vertex-attrib-array 1)
        (gl:vertex-attrib-pointer 1 2 :float nil 0 (cffi:null-pointer)))
      (gl:bind-buffer :element-array-buffer index-buffer)
      (gl:bind-vertex-array 0)

      ;; set that shit
      (setf (gl-object-vertex-data gl-object) vertex-array
            (gl-object-index-data gl-object) index
            (getf (gl-object-vbos gl-object) :vertex) vertex-buffer
            (getf (gl-object-vbos gl-object) :index) index-buffer
            (gl-object-texture gl-object) texture)))
  gl-object)

(defun update-gl-object-vertex-data (gl-object vertex-data)
  "Given a set of raw vertex data, reset the buffer objects in the gl object so
  the new data is reflected. New data *must* be the same size as the old data."
  (let ((vertex-buffer (getf (gl-object-vbos gl-object) :vertex)))
    (gl:bind-buffer :array-buffer vertex-buffer)
    (let ((gl-arr (gl:alloc-gl-array :float (length vertex-data))))
      (dotimes (i (length vertex-data))
        (setf (gl:glaref gl-arr i) (coerce (aref vertex-data i) 'single-float)))
      (gl:buffer-data :array-buffer :static-draw gl-arr)
      (gl:free-gl-array gl-arr))
    (gl:bind-buffer :array-buffer 0)))

(defun draw-gl-object (obj &key color position)
  (let* ((position (if position position (gl-object-position obj)))
         (rotation (gl-object-rotation obj))
         (scale (gl-object-scale obj))
         (model-matrix (id-matrix 4))
         (model-matrix (mat* model-matrix (apply #'m-rotate rotation)))
         (model-matrix (mat* model-matrix (apply #'m-translate position)))
         (model-matrix (mat* model-matrix (apply #'m-scale scale)))
         (model-matrix (mat* model-matrix *view-matrix*))
         (mv-matrix (mat* model-matrix *view-matrix*)))
    (set-shader-matrix "modelToCameraMatrix" mv-matrix))
  (let ((color (if color color (gl-object-color obj))))
    (set-shader-var #'gl:uniformfv "colorIn" color))
  (gl:bind-vertex-array (gl-object-vao obj))
  (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-short) :count (length (gl-object-index-data obj)))
  (gl:bind-vertex-array 0))

(defun create-point-index (triangles)
  (let ((point-set (make-hash-table :test #'equal))
        (index nil)
        (counter 0))
    (dolist (triangle triangles)
      (dolist (vertex triangle)
        (multiple-value-bind (idx exists) (gethash vertex point-set)
          (if exists
              (push idx index)
              (progn (setf (gethash vertex point-set) counter)
                     (push counter index)
                     (incf counter))))))
    (let ((points (make-array (hash-table-count point-set) :initial-element nil)))
      (maphash (lambda (point idx) (setf (aref points idx) point)) point-set)
      (values points (coerce (reverse index) 'vector)))))

(defun flatten-vertices-into-array (vertices)
  (let* ((points-per-vert (length (aref vertices 0)))
         (num-verts (length vertices))
         (vert-array (make-array (* 3 num-verts) :initial-element 0))
         (idx 0))
    (loop for vertex across vertices do
      (dolist (coord vertex)
        (setf (aref vert-array idx) coord)
        (incf idx))
      (incf idx (- 3 points-per-vert)))
    vert-array))

(defmethod free-gl-object (gl-object)
  (when (gl-object-texture gl-object)
    (gl:delete-textures (list (gl-object-texture gl-object))))
  (when (gl-object-vbos gl-object)
    (gl:delete-buffers
      (loop for (nil vbo) on (gl-object-vbos gl-object) by #'cddr collect vbo))
    (setf (gl-object-vbos gl-object) nil))
  (when (gl-object-vao gl-object)
    (gl:delete-vertex-arrays (list (gl-object-vao gl-object)))
    (setf (gl-object-vao gl-object) nil)))

  ;(gl:enable-vertex-attrib-array 0)
  ;(gl:vertex-attrib-pointer 0 4 :float :false 0 (cffi:null-pointer))
  ;(gl:draw-arrays :triangles 0 3)
  ;(gl:disable-vertex-attrib-array 0)
  ;(gl:bind-buffer :element-array-buffer 0)
  ;(gl:bind-buffer :array-buffer 0)
  ;(gl:bind-buffer :array-buffer (gl-object-vertex-buffer obj))
  ;(gl:enable-vertex-attrib-array 0)
  ;(gl:vertex-attrib-pointer 0 4 :float :false 0 (cffi:null-pointer))
  ;(gl:draw-arrays :triangles 0 3)
  ;(gl:bind-buffer :array-buffer 0)
