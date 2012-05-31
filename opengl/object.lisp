(in-package :game-level)

(defclass gl-object ()
  ((data :accessor gl-object-data :initarg :data :initform nil)
   (buffer :accessor gl-object-buffer :initform nil)))

(defun make-gl-object (&key data)
  (set-gl-object-data (make-instance 'gl-object) data))

(defmethod set-gl-object-data (gl-object (vertex-array vector))
  "Copies a set of floating-point vertex data into a VBO which is then stored
  with the gl-object."
  (free-gl-object-buffer gl-object)
  (let ((buffer (car (gl:gen-buffers 1)))
        (gl-arr (gl:alloc-gl-array :float (length vertex-array))))
    (dotimes (i (length vertex-array))
      (setf (gl:glaref gl-arr i) (aref vertex-array i)))
    (gl:bind-buffer :array-buffer buffer)
    (gl:buffer-data :array-buffer :static-draw gl-arr)
    (gl:free-gl-array gl-arr)
    (gl:bind-buffer :array-buffer 0)
    (setf (gl-object-buffer gl-object) buffer
          (gl-object-data gl-object) vertex-array))
  gl-object)

(defun create-point-index (triangles)
  (let ((point-set (make-hash-table :test #'equalp))
        (index nil)
        (counter 0))
    (dolist (triangle triangles)
      (dolist (vertex triangle)
        (push counter index)
        (unless (cadr (multiple-value-list (gethash vertex point-set)))
          (setf (gethash vertex point-set) counter)
          (incf counter))))
    (let ((points (make-array (hash-table-size point-set))))
      (loop for point being the hash-keys of point-set
            for index being the hash-values of point-set do
        (setf (aref points index) point))
      (values points index))))



(defmethod free-gl-object-buffer (gl-object)
  (when (gl-object-buffer gl-object)
    (gl:delete-buffers (list (gl-object-buffer gl-object)))
    (setf (gl-object-buffer gl-object) nil)))
