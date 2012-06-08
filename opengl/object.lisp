(in-package :game-level)

(defclass gl-object ()
  ((vertex-data :accessor gl-object-vertex-data :initarg :vertex-data :initform nil)
   (index-data :accessor gl-object-index-data :initarg :index-data :initform nil)
   (scale :accessor gl-object-scale :initarg :scale :initform #(1 1 1))
   (position :accessor gl-object-position :initarg :position :initform '(0 0 0))
   (rotation :accessor gl-object-rotation :initarg :rotation :initform '(0 0 0 0))
   (vao :accessor gl-object-vao :initform nil)
   (vertex-buffer :accessor gl-object-vertex-buffer :initform nil)
   (index-buffer :accessor gl-object-index-buffer :initform nil)))

(defun make-gl-object (&key data position)
  (set-gl-object-data (make-instance 'gl-object :position position) data))

(defmethod set-gl-object-data (gl-object (triangles list))
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
      (gl:bind-buffer :element-array-buffer index-buffer)
      (gl:bind-vertex-array 0)

      ;; set that shit
      (setf (gl-object-vertex-data gl-object) vertex-array
            (gl-object-index-data gl-object) index
            (gl-object-vertex-buffer gl-object) vertex-buffer
            (gl-object-index-buffer gl-object) index-buffer)))
  gl-object)

(defmethod draw ((obj gl-object))
  (let* ((position (gl-object-position obj))
         (rotation (gl-object-rotation obj))
         (model-matrix (id-matrix 4))
         (model-matrix (mat* model-matrix (apply #'m-rotate rotation)))
         (model-matrix (mat* model-matrix (apply #'m-translate position)))
         (model-matrix (mat* model-matrix *view-matrix*))
         (mv-matrix (mat* model-matrix *view-matrix*)))
    (gl:uniform-matrix *model-to-camera-matrix-unif* 4 (vector mv-matrix) t))
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
  (when (gl-object-index-buffer gl-object)
    (gl:delete-buffers (list (gl-object-index-buffer gl-object)))
    (setf (gl-object-index-buffer gl-object) nil))
  (when (gl-object-vertex-buffer gl-object)
    (gl:delete-buffers (list (gl-object-vertex-buffer gl-object)))
    (setf (gl-object-vertex-buffer gl-object) nil))
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
